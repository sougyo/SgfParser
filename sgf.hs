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
    bracketBlock = try $ between (char '[') (char ']') $ BracketBlock <$> text
    ucWord       = UcWord <$> many1 upper
    text         = fmap concat $ many $ try (string "\\]") <|> (return <$> noneOf "]")

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
    gameTree     = between (token LeftParenthes) (token RightParenthes) $
                      makeGameTree <$> many1 node <*> many gameTree
    node         = token Semicolon *> many property
    property     = do ident <- propIdent
                      pvals <- many1 propValue
                      case s2v ident pvals of
                        Just x  -> return $ SProp ident x
                        Nothing -> fail $ "failed to parse " ++ (show pvals)
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

numstr_parser = (++) <$> plus_minus <*> many1 digit
  where plus_minus = option "" $ fmap return $ oneOf "+-"

realstr_parser = (++) <$> numstr_parser <*> decimal_places
  where decimal_places = option "" $ (:) <$> char '.' <*> many1 digit

v_number_parser = VNumber . read <$> numstr_parser <* eof

v_real_parser = VReal . read <$> realstr_parser <* eof

number_parser = runValueParser v_number_parser

real_parser = runValueParser v_real_parser

double_parser = runValueParser $ fmap VDouble $
  char '1' *> return VOne <|>
  char '2' *> return VTwo

color_parser = runValueParser $ fmap VColor $
  char 'B' *> return VBlack <|>
  char 'W' *> return VWhite

v_move_parser = fmap VMove $ (,) <$> letter <*> letter

move_parser :: String -> Maybe (ValueType () (Char, Char) ())
move_parser = runValueParser v_move_parser

eol = try (string "\n\r") <|>
      try (string "\r\n") <|>
      string "\n"         <|>
      string "\r"

v_text_parser = VText <$> concat <$> many hoge
  where
    hoge = try (char '\\' *> eol *> return "")     <|>
           try (char '\\' *> (return <$> anyChar)) <|>
           return <$> noneOf "]"

text_parser = runValueParser v_text_parser

v_stext_parser = VSimpleText <$> concat <$> many hoge
  where
    hoge = try (char '\\' *> eol *> return "")     <|>
           try (char '\\' *> space *> return " ")  <|>
           try (char '\\' *> (return <$> anyChar)) <|>
           try (space *> return " ") <|>
           return <$> noneOf "]"

stext_parser = runValueParser v_stext_parser

just_one p s = when (length s /= 1) Nothing >> (join . Just . p . head $ s)
list_of  p s = fmap VList $ foldr (\x y -> x >>= \z -> fmap (z:) y) (Just []) $ map p s
elist_of p s = if length s == 1 && null (head s) then Just VNone
               else list_of p s

composition_of p1 p2 = fmap VPair $ (,) <$> p1 <* (char ':') <*> p2

cof_point_point = composition_of v_move_parser        v_move_parser
cof_point_stext = composition_of v_move_parser        v_stext_parser
cof_stext_stext = composition_of v_stext_parser v_stext_parser
cof_num_num     = composition_of v_number_parser      v_number_parser
cof_num_stext   = composition_of v_number_parser      v_stext_parser

list_of_stone _ = Nothing
list_of_point _ = Nothing
elist_of_point _ = Nothing
list_of_composition_point_point _ = Nothing
list_of_composition_point_stext _ = Nothing
list_of_composition_stext_stext _ = Nothing
number_or_composition_number_number _ = Nothing
none_or_composition_number_stext _ = Nothing

str2val dict ident strs = case (lookup ident dict) of
    Just p  -> p strs
    Nothing -> Nothing

parser_dict = [
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
    ("N" , just_one stext_parser),
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
    ("LB", list_of_composition_point_stext),
    ("LN", list_of_composition_point_point),
    ("MA", list_of_point),
    ("SL", list_of_point),
    ("SQ", list_of_point),
    ("TR", list_of_point),

    -- Root properties
    ("AP", list_of_composition_stext_stext),
    ("CA", just_one stext_parser),
    ("FF", just_one number_parser),
    ("GM", just_one number_parser),
    ("ST", just_one number_parser),
    ("SZ", number_or_composition_number_number),

    -- Game info properties
    ("AN", just_one stext_parser),
    ("BR", just_one stext_parser),
    ("BT", just_one stext_parser),
    ("CP", just_one stext_parser),
    ("DT", just_one stext_parser),
    ("EV", just_one stext_parser),
    ("GN", just_one stext_parser),
    ("GC", just_one text_parser),
    ("ON", just_one stext_parser),
    ("OT", just_one stext_parser),
    ("PB", just_one stext_parser),
    ("PC", just_one stext_parser),
    ("PW", just_one stext_parser),
    ("RE", just_one stext_parser),
    ("RO", just_one stext_parser),
    ("RU", just_one stext_parser),
    ("SO", just_one stext_parser),
    ("TM", just_one real_parser),
    ("US", just_one stext_parser),
    ("WR", just_one stext_parser),
    ("WT", just_one stext_parser),

    -- Timing properties
    ("BL", just_one real_parser),
    ("OB", just_one number_parser),
    ("OW", just_one number_parser),
    ("WL", just_one real_parser),

    -- Timing properties
    ("FG", none_or_composition_number_stext),
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

parseSgf input = case parse (sgfParser (str2val parser_dict)) "" input of
  Left  err -> putStrLn $ show err
  Right val -> putStrLn $ show $ val

main = parseSgf $ parseSgfToken "(;B[cc](;W[aa])(;US[hoge\nhoge]))"
