module SgfParser
  ( SProp(..),
    SgfTreeNode(..),
    VDoubleType(..),
    VColorType(..),
    ValueType(..),
    just_one,
    list_of,
    elist_of,
    point_list_of,
    point_elist_of,
    compose,
    none_parser,
    number_parser,
    real_parser,
    double_parser,
    color_parser,
    text_parser,
    stext_parser,
    c_stext_parser,
    base_dict,
    parseSgf
  ) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)
import Control.Monad (sequence, when)

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

data SProp a = SProp {
  s_ident  :: String,
  s_blocks :: a
} deriving (Show)

data SgfTreeNode a = SgfTreeNode {
  s_node     :: [SProp a],
  s_children :: [SgfTreeNode a]
}

data VDoubleType = VOne | VTwo
  deriving (Show, Eq)

data VColorType = VBlack | VWhite
  deriving (Show, Eq)

data ValueType p m s =
    VNone                                    |
    VNumber     Integer                      |
    VReal       Double                       |
    VDouble     VDoubleType                  |
    VColor      VColorType                   |
    VText       String                       |
    VSimpleText String                       |
    VPoint      p                            |
    VMove       m                            |
    VStone      s                            |
    VList [ValueType p m s]                  |
    VPair (ValueType p m s, ValueType p m s) |
    VParseError  [String]                    |
    VUnknownProp [String]
  deriving (Eq)

--

sgfParser dict = SgfTreeNode [] <$> many1 gameTree
  where
    gameTree  = between (token LeftParenthes) (token RightParenthes) $
                  makeGameTree <$> many1 node <*> many gameTree
    node      = token Semicolon *> many property
    property  = do ident <- propIdent
                   pvals <- many1 propValue
                   fmap (SProp ident) $ return $ case lookup ident dict of
                     Just p  -> case p pvals of
                       Just x  -> x
                       Nothing -> VParseError pvals
                     Nothing -> VUnknownProp pvals
    makeGameTree (n:ns) ts
      | null ns   = SgfTreeNode n ts
      | otherwise = SgfTreeNode n [makeGameTree ns ts]
    gen_p m   = tokenPrim (\c -> show c) (\pos c _cs -> m_position c) m
    token c   = gen_p $ \n -> if m_token n == c then Just n else Nothing
    propIdent = gen_p $ \n -> case m_token n of
                  UcWord s -> Just s
                  _        -> Nothing
    propValue = gen_p $ \n -> case m_token n of
                  BracketBlock s -> Just s
                  _              -> Nothing

--

base_dict point_parser move_parser stone_parser = [
    -- Move Properties
    ("B" , just_one move_parser),
    ("KO", just_one none_parser),
    ("MN", just_one number_parser),
    ("W" , just_one move_parser),

    -- Setup Properties
    ("AB", list_of stone_parser),
    ("AE", list_of stone_parser),
    ("AW", list_of stone_parser),
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
    ("AR", list_of $ compose point_parser point_parser),
    ("CR", point_list_of point_parser),
    ("DD", point_elist_of point_parser),
    ("LB", list_of $ compose point_parser c_stext_parser),
    ("LN", list_of $ compose point_parser point_parser),
    ("MA", point_list_of point_parser),
    ("SL", point_list_of point_parser),
    ("SQ", point_list_of point_parser),
    ("TR", point_list_of point_parser),

    -- Root properties
    ("AP", list_of $ compose c_stext_parser c_stext_parser),
    ("CA", just_one stext_parser),
    ("FF", just_one number_parser),
    ("GM", just_one number_parser),
    ("ST", just_one number_parser),
    ("SZ", just_one $ try (compose number_parser number_parser) <|> number_parser),

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
    ("FG", just_one $ try (compose number_parser c_stext_parser) <|> none_parser),
    ("PM", just_one number_parser),
    ("VW", point_elist_of point_parser)
  ]

--

none_parser = string "" *> return VNone

number_parser = VNumber . read <$> numstr_parser

real_parser = fmap (VReal . read) $ (++) <$> numstr_parser <*> decimal_places
  where decimal_places = option "" $ (:) <$> char '.' <*> many1 digit

double_parser = fmap VDouble $
  char '1' *> return VOne <|>
  char '2' *> return VTwo

color_parser = fmap VColor $
  char 'B' *> return VBlack <|>
  char 'W' *> return VWhite

text_parser    = VText       <$> concat <$> many (text_parser_base "]\\"  True)

stext_parser   = VSimpleText <$> concat <$> many (text_parser_base "]\\"  False)

c_stext_parser = VSimpleText <$> concat <$> many (text_parser_base "]\\:" False)

--

just_one p s = when (length s /= 1) Nothing >> (parseMaybe p $ head s)

list_of  p s = fmap VList $ sequence $ map (parseMaybe p) s

elist_of p s = if length s == 1 && null (head s) then Just VNone else list_of p s

point_list_of  p = list_of  $ point_list_elem p

point_elist_of p = elist_of $ point_list_elem p

compose p1 p2 = fmap VPair $ (,) <$> p1 <* (char ':') <*> p2

--

parseMaybe p s = case parse (p <* eof) "" s of
  Left  err -> Nothing
  Right val -> Just val

numstr_parser = (++) <$> plus_minus <*> many1 digit
  where plus_minus = try (char '+' *> return "") <|>
                     option "" (string "-")

text_parser_base escape_chars keep_eol = 
    try (bslash *> eol *> return "")     <|>
    try (bslash *> space *> return " ")  <|>
    try (bslash *> (return <$> anyChar)) <|>
    try (eol_parser keep_eol)            <|>
    try (space *> return " ")            <|>
    return <$> noneOf escape_chars
  where
      bslash = char '\\'
      eol_parser keep_eol
        | keep_eol   = eol
        | otherwise  = return <$> oneOf ""
      eol = try (string "\n\r") <|>
            try (string "\r\n") <|>
            string "\n"         <|>
            string "\r"

point_list_elem p = try (compose p p) <|> p

--

parseSgf dict input = case parse (spaces *> sgfTokenParser <* eof) "" input of
                        Left  err -> Left err
                        Right val -> parse (sgfParser dict) "" val
  where
    sgfTokenParser = many1 $ MNode <$> getPosition <*> mToken <* spaces
    mToken = mtoken '(' LeftParenthes  <|>
             mtoken ')' RightParenthes <|>
             mtoken ';' Semicolon      <|>
             bracketBlock              <|>
             ucWord
    mtoken c m   = try $ char c *> return m
    bracketBlock = try $ between (char '[') (char ']') $ BracketBlock <$> text
    ucWord       = UcWord <$> many1 upper
    text         = fmap concat $ many $ try (string "\\]") <|> (return <$> noneOf "]")

