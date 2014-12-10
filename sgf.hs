import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)
import Data.List (intercalate)
import Control.Monad (join, when)

data MToken = LeftParenthes       |
              RightParenthes      |
              Semicolon           |
              BracketBlock String |
              UcWord String
  deriving (Show, Eq)

data MNode = MNode {
  m_position :: SourcePos,
  m_token    :: MToken
} deriving (Show, Eq)

mToken = mtoken '(' LeftParenthes  <|>
         mtoken ')' RightParenthes <|>
         mtoken ';' Semicolon      <|>
         bracketBlock              <|>
         ucWord
  where
    mtoken c m   = try $ char c *> return m
    bracketBlock = try $ fmap BracketBlock $ char '[' *> text <* char ']'
    ucWord       = fmap UcWord $ many1 upper
    text         = many $ noneOf "]"

data SProp a = SProp {
  s_ident  :: String,
  s_blocks :: a
} deriving (Show)

data SgfTreeNode a = SgfTreeNode {
  s_node     :: [SProp a],
  s_children :: [SgfTreeNode a]
}

sgfParser s2v = SgfTreeNode [] <$> many1 gameTree
  where
    gameTree     = do token LeftParenthes
                      ns <- many1 node
                      ts <- many gameTree
                      token RightParenthes
                      return $ makeGameTree ns ts
    node         = token Semicolon *> many property
    property     = do ident <- propIdent
                      pvals <- many1 propValue
                      case s2v ident pvals of
                        Just x  -> return $ SProp ident x
                        Nothing -> fail "hoge"
    makeGameTree (n:ns) ts
     | null ns   = SgfTreeNode n ts
     | otherwise = SgfTreeNode n [makeGameTree ns ts]
    gen_p m      = tokenPrim (\c -> show c) (\pos c _cs -> m_position c) m
    token c      = gen_p $ \n -> if m_token n == c then Just n else Nothing
    propIdent    = gen_p $ \n -> case m_token n of
                     UcWord s -> Just s
                     _        -> Nothing
    propValue    = gen_p $ \n -> case m_token n of
                     BracketBlock s -> Just s
                     _              -> Nothing



data VDoubleType = VOne | VTwo
  deriving (Show, Eq)

data VColorType = VBlack | VWhite
  deriving (Show, Eq)

data ValueType a b c =
    VNone                   |
    VNumber     Integer     |
    VReal       Double      |
    VDouble     VDoubleType |
    VColor      VColorType  |
    VSimpleText String      |
    VPoint      a           |
    VMove       b           |
    VStone      c           |
    VCompose    [ValueType a b c] |
    VPair       (ValueType a b c, ValueType a b c)
  deriving (Show, Eq)

runValueParser p s = case parse p "" s of
  Left  err -> Nothing
  Right val -> Just val

any_parser _ = Just VNone
none_parser str = if null str then Just VNone else Nothing

number_parser str = runValueParser _parser str
  where
    _parser = fmap VNumber $ fmap (\x -> ((read x) :: Integer)) $ many1 digit <* eof

real_parser str = runValueParser _parser str
  where
    _parser = fmap VReal $ fmap (\x -> ((read x) :: Double)) $ many1 digit <* eof

double_parser str
  | str == "1" = Just $ VDouble VOne
  | str == "2" = Just $ VDouble VTwo
  | otherwise  = Nothing

color_parser str
  | str == "B" = Just $ VColor VBlack
  | str == "W" = Just $ VColor VWhite
  | otherwise  = Nothing

move_parser :: String -> Maybe (ValueType () (Int, Int) ())
move_parser str = runValueParser _parser str
  where
    _parser = fmap VMove $ fmap (\(x, y) -> (0, 0)) $ do
                a <- letter
                b <- letter
                eof
                return (a, b)

simple_text_parser str = Just $ VSimpleText str

str2val :: String -> [String] -> Maybe (ValueType () (Int, Int) ())
str2val ident strs = case (lookup ident parser_dict) of
    Just f  -> Just VNone
    Nothing -> Nothing
  where parser_dict = [
              ("B" , move_parser),
              ("KO", move_parser),
              ("MN", move_parser),
              ("W" , move_parser),
              ("PL", color_parser),
              ("DM", double_parser),
              ("GB", double_parser),
              ("GW", double_parser),
              ("HO", double_parser),
              ("N" , simple_text_parser),
              ("UC", double_parser),
              ("V" , real_parser),
              ("BM", double_parser),
              ("DO", none_parser),
              ("IT", move_parser),
              ("TE", double_parser),
              ("CA", simple_text_parser),
              ("FF", number_parser),
              ("GM", number_parser),
              ("ST", number_parser),
              ("AN", simple_text_parser),
              ("BR", simple_text_parser),
              ("BT", simple_text_parser),
              ("CP", simple_text_parser),
              ("DT", simple_text_parser),
              ("EV", simple_text_parser),
              ("GN", simple_text_parser)
            ]


data Hoge a = Hoge {
  h_dx   :: Int,
  h_dy   :: Int,
  h_node :: [SProp a]
} deriving (Show)

instance (Show a) => Show (SgfTreeNode a) where
  show tn = show_matrix $ make_matrix tn
    where
      make_matrix tn = helper1 0 0 tn
      helper1 dx dy (SgfTreeNode n ts) = (:) (Hoge dx dy n) $ concat $ helper2 dx (dy + 1) ts
      helper2 _  _  []     = []
      helper2 dx dy (t:ts) = let z = helper1 dx dy t
                             in (:) z $ helper2 (max_dx z + 1) dy ts
      max_dx z = foldr max 0 $ map h_dx z
      show_matrix hoges = helper3 0 hoges
      helper3 d hoges = let filtered = filter (\x -> h_dy x == d) hoges
                        in if null filtered then "" else 
                           show_filtered filtered ++ "\n" ++ (helper3 (d + 1) hoges)
      show_snode ps = intercalate ":" $ map (show . s_ident) ps
      show_filtered filtered = helper4 0 filtered
      helper4 _ []     = ""
      helper4 d (f:fs) = if d < 10 * h_dx f then " " ++ helper4 (d + 1) (f:fs)
                         else let str = show_snode . h_node $ f
                              in str ++ helper4 (d + length str) fs


parseSgfToken input = case parse sgfTokenParser "" input of
     Left err  -> []
     Right val -> val
  where 
    sgfTokenParser = many1 $ spaces *> mnToken
    mnToken = MNode <$> getPosition <*> mToken

parseSgf input = case parse (sgfParser str2val) "" input of
  Left  err -> putStrLn $ show err
  Right val -> putStrLn $ show $ val

main = parseSgf $ parseSgfToken "(;B[cc];W[aa];B[ww](;DO[][]))"
