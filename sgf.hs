import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)

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

isBracketBlock (BracketBlock _) = True
isBracketBlock _                = False

isUcWord (UcWord _) = True
isUcWord _          = False

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

nSatisfy f =
  tokenPrim (\c -> show c)
  (\pos c _cs -> m_position c)
  (\c -> if f c then Just c else Nothing)

data SColor = SB | SW
  deriving (Show, Eq)

data SValueType = SNone           |
                  SNumber Integer |
                  SText String    |
                  SPoint Int Int
  deriving (Show, Eq)

data SProp = SProp String [MNode]
  deriving (Show)

data SNode = SNode [SProp]
  deriving (Show)

data SgfTreeNode = SgfTreeNode SNode [SgfTreeNode]
  deriving (Show)

--getParser ident = MNode <$> getPosition <*> (fmap BracketBlock $ many1 digit)
getParser _ = nSatisfy $ \_ -> True  -- for debug

sgfParser = many1 gameTree
  where
    gameTree     = do token LeftParenthes
                      ns <- many1 node
                      ts <- many gameTree
                      token RightParenthes
                      return $ makeGameTree ns ts
    node         = token Semicolon *> (fmap SNode $ many property)
    property     = propIdent >>= \i -> fmap (SProp i) $ many1 (getParser i)
    propIdent    = fmap ucword2s $ tSatisfy isUcWord
    ucword2s t   = case m_token t of UcWord w -> w
    block2s  b   = case m_token b of BracketBlock s -> s
    token c      = nSatisfy $ \x -> m_token x == c
    tSatisfy f   = nSatisfy $ \x -> f $ m_token x
    makeGameTree (n:ns) ts
     | null ns   = SgfTreeNode n ts
     | otherwise = SgfTreeNode n [makeGameTree ns ts]

parseSgfToken input = case parse sgfTokenParser "" input of
     Left err  -> []
     Right val -> val
  where 
    sgfTokenParser = many1 $ spaces *> mnToken
    mnToken = MNode <$> getPosition <*> mToken

parseSgf input = case parse sgfParser "" input of
  Left  err -> putStrLn $ show err
  Right val -> putStrLn $ show val

main = parseSgf $ parseSgfToken "(;MN[333])"
