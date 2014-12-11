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

data ValueType p m s =
    VNone                   |
    VNumber     Integer     |
    VReal       Double      |
    VDouble     VDoubleType |
    VColor      VColorType  |
    VText       String      |
    VSimpleText String      |
    VPoint      p           |
    VMove       m           |
    VStone      s           |
    VList [ValueType p m s] |
    VPair (ValueType p m s, ValueType p m s)
  deriving (Show, Eq)

runValueParser p s = case parse p "" s of
  Left  err -> Nothing
  Right val -> Just val

none_parser = just_one $ \str -> if null str then Just VNone else Nothing

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


move_parser str = runValueParser _parser str
  where
    _parser = fmap VMove $ fmap (\(x, y) -> (0, 0)) $ do
                a <- letter
                b <- letter
                eof
                return (a, b)

text_parser str = Just $ VText str
simple_text_parser str = Just $ VSimpleText str

just_one p s = if length s == 1 then join . Just . p . head $ s else Nothing
list_of  p s = fmap VList $ foldr (\x y -> x >>= \z -> fmap (z:) y) (Just []) $ map p s
elist_of p s = if length s == 1 && null (head s) then Just VNone
               else list_of p s

list_of_stone _ = Nothing
list_of_point _ = Nothing
elist_of_point _ = Nothing
list_of_composition_point_point _ = Nothing
list_of_composition_point_simple_text _ = Nothing
list_of_composition_simple_text_simple_text _ = Nothing
number_or_composition_number_number _ = Nothing
none_or_composition_number_simple_text _ = Nothing

str2val :: String -> [String] -> Maybe (ValueType () (Int, Int) ())
str2val ident strs = case (lookup ident parser_dict) of
    Just p  -> p strs
    Nothing -> Nothing
  where parser_dict = [
              -- Move Properties
              ("B" , just_one move_parser),
              ("KO", none_parser),
              ("MN", just_one number_parser),
              ("W" , just_one move_parser),

              -- Setup Properties
              ("AB", list_of_stone),
              ("AE", list_of_stone),
              ("AW", list_of_stone),
              ("PL", just_one color_parser),
              
              -- Node annotation properties
              ("C" , just_one text_parser),
              ("DM", just_one double_parser),
              ("GB", just_one double_parser),
              ("GW", just_one double_parser),
              ("HO", just_one double_parser),
              ("N" , just_one simple_text_parser),
              ("UC", just_one double_parser),
              ("V" , just_one real_parser),

              -- Move annotation properties
              ("BM", just_one double_parser),
              ("DO", none_parser),
              ("IT", just_one move_parser),
              ("TE", just_one double_parser),

              -- Markup properties
              ("AR", list_of_composition_point_point),
              ("CR", list_of_point),
              ("DD", elist_of_point),
              ("LB", list_of_composition_point_simple_text),
              ("LN", list_of_composition_point_point),
              ("MA", list_of_point),
              ("SL", list_of_point),
              ("SQ", list_of_point),
              ("TR", list_of_point),

              -- Root properties
              ("AP", list_of_composition_simple_text_simple_text),
              ("CA", just_one simple_text_parser),
              ("FF", just_one number_parser),
              ("GM", just_one number_parser),
              ("ST", just_one number_parser),
              ("SZ", number_or_composition_number_number),

              -- Game info properties
              ("AN", just_one simple_text_parser),
              ("BR", just_one simple_text_parser),
              ("BT", just_one simple_text_parser),
              ("CP", just_one simple_text_parser),
              ("DT", just_one simple_text_parser),
              ("EV", just_one simple_text_parser),
              ("GN", just_one simple_text_parser),
              ("GC", just_one text_parser),
              ("ON", just_one simple_text_parser),
              ("OT", just_one simple_text_parser),
              ("PB", just_one simple_text_parser),
              ("PC", just_one simple_text_parser),
              ("PW", just_one simple_text_parser),
              ("RE", just_one simple_text_parser),
              ("RO", just_one simple_text_parser),
              ("RU", just_one simple_text_parser),
              ("SO", just_one simple_text_parser),
              ("TM", just_one real_parser),
              ("US", just_one simple_text_parser),
              ("WR", just_one simple_text_parser),
              ("WT", just_one simple_text_parser),

              -- Timing properties
              ("BL", just_one real_parser),
              ("OB", just_one number_parser),
              ("OW", just_one number_parser),
              ("WL", just_one real_parser),

              -- Timing properties
              ("FG", none_or_composition_number_simple_text),
              ("PM", just_one number_parser),
              ("VW", elist_of_point)
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

main = parseSgf $ parseSgfToken "(;B[cc](;W[aa])(;W[cc]))"
