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

runValueParser p s = case parse (p <* eof) "" s of
  Left  err -> Nothing
  Right val -> Just val

just_one p s = when (length s /= 1) Nothing >> (runValueParser p $ head s)
list_of  p s = fmap VList $ foldr (\x y -> x >>= \z -> fmap (z:) y) (Just []) $ map (runValueParser p) s
elist_of p s = if length s == 1 && null (head s) then Just VNone
               else list_of p s

eol = try (string "\n\r") <|>
      try (string "\r\n") <|>
      string "\n"         <|>
      string "\r"

none_parser = string "" *> return VNone

numstr_parser = (++) <$> plus_minus <*> many1 digit
  where plus_minus = option "" $ fmap return $ oneOf "+-"

number_parser = VNumber . read <$> numstr_parser

real_parser = fmap (VReal . read) $ (++) <$> numstr_parser <*> decimal_places
  where decimal_places = option "" $ (:) <$> char '.' <*> many1 digit

double_parser = fmap VDouble $
  char '1' *> return VOne <|>
  char '2' *> return VTwo

color_parser = fmap VColor $
  char 'B' *> return VBlack <|>
  char 'W' *> return VWhite

text_parser = VText <$> concat <$> many hoge
  where
    hoge = try (char '\\' *> eol *> return "")     <|>
           try (char '\\' *> (return <$> anyChar)) <|>
           return <$> noneOf "]"

stext_parser = VSimpleText <$> concat <$> many hoge
  where
    hoge = try (char '\\' *> eol *> return "")     <|>
           try (char '\\' *> space *> return " ")  <|>
           try (char '\\' *> (return <$> anyChar)) <|>
           try (space *> return " ") <|>
           return <$> noneOf "]"

move_parser  = fmap VMove $ (,) <$> letter <*> letter

stone_parser = fmap VStone $ (,) <$> letter <*> letter

point_parser = fmap VPoint $ (,) <$> letter <*> letter

lpoint_parser = try (composition_of point_parser point_parser) <|>
                point_parser

composition_of p1 p2 = fmap VPair $ (,) <$> p1 <* (char ':') <*> p2

list_of_stone    = list_of  stone_parser
list_of_point    = list_of  lpoint_parser
elist_of_point   = elist_of lpoint_parser
lofc_point_point = list_of $ composition_of lpoint_parser lpoint_parser
lofc_point_stext = list_of $ composition_of lpoint_parser stext_parser
lofc_stext_stext = list_of $ composition_of stext_parser  stext_parser
sz_parser        = try (composition_of number_parser number_parser) <|> number_parser
fg_parser        = try (composition_of number_parser stext_parser) <|> none_parser

type IgoMoveType = (Char, Char)
type IgoType = ValueType IgoMoveType IgoMoveType IgoMoveType

str2val :: (Eq a) => [(a, [String] -> Maybe IgoType)] -> a -> [String] -> Maybe IgoType
str2val dict ident strs = case (lookup ident dict) of
    Just p  -> p strs
    Nothing -> Nothing

parser_dict = [
    -- Move Properties
    ("B" , just_one move_parser),
    ("KO", just_one none_parser),
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
    ("DO", just_one none_parser),
    ("IT", just_one move_parser),
    ("TE", just_one double_parser),

    -- Markup properties
    ("AR", lofc_point_point),
    ("CR", list_of_point),
    ("DD", elist_of_point),
    ("LB", lofc_point_stext),
    ("LN", lofc_point_point),
    ("MA", list_of_point),
    ("SL", list_of_point),
    ("SQ", list_of_point),
    ("TR", list_of_point),

    -- Root properties
    ("AP", lofc_stext_stext),
    ("CA", just_one stext_parser),
    ("FF", just_one number_parser),
    ("GM", just_one number_parser),
    ("ST", just_one number_parser),
    ("SZ", just_one sz_parser),

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
    ("FG", just_one fg_parser),
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
