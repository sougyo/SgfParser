import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)

data MToken = LeftParenthes       |
              RightParenthes      |
              Semicolon           |
              BracketBlock String |
              PropIdent String
  deriving (Show, Eq)

data MNode = MNode {
  m_position :: SourcePos,
  m_token    :: MToken
} deriving (Show, Eq)

isBracketBlock (BracketBlock _) = True
isBracketBlock _                = False

isPropIdent (PropIdent _) = True
isPropIdent _             = False

mToken = terminate '(' LeftParenthes   <|>
         terminate ')' RightParenthes  <|>
         terminate ';' Semicolon       <|>
         try bracketBlock              <|>
         propIdentity
  where
    terminate c m = try $ char c *> return m
    bracketBlock = fmap BracketBlock $ char '[' *> many (noneOf "[]") <* char ']'
    propIdentity = many1 upper >>= \s -> return $ PropIdent s

nSatisfy f =
  tokenPrim (\c -> show c)
  (\pos c _cs -> m_position c)
  (\c -> if f c then Just c else Nothing)

tSatisfy f = nSatisfy $ \x -> f $ m_token x

tElem c = nSatisfy $ \x -> (m_token x) == c
tProp s = nSatisfy $ \x -> case (m_token x) of
                               PropIdent s' -> s == s'
                               _            -> False

data SProp = SProp MNode [MNode]
  deriving (Show)

data SNode = SNode [SProp] | SNull
  deriving (Show)

data SgfTreeNode = SgfTreeNode SNode [SgfTreeNode]
  deriving (Show)

node_tree_to_node (x:xs) yss = if null xs then SgfTreeNode x yss
                               else SgfTreeNode x [node_tree_to_node xs yss]

sgfParser = do many1 s_gameTree
  where
    s_gameTree = do tElem LeftParenthes
                    s <- s_sequence
                    t <- many s_gameTree
                    tElem RightParenthes
                    return $ node_tree_to_node s t
    s_sequence = many1 s_node
    s_node     = tElem Semicolon *> (fmap SNode $ many s_property)
    s_property = SProp <$> tSatisfy isPropIdent <*> many1 s_propValue
    s_propValue = tSatisfy isBracketBlock

sgfTokenParser = many1 $ spaces *> mnToken
  where
    mnToken = MNode <$> getPosition <*> mToken

parseSgfToken input = case parse sgfTokenParser "" input of
     Left err  -> []
     Right val -> val

parseSgf input = case parse sgfParser "" input of
  Left  err -> putStrLn $ show err
  Right val -> putStrLn $ show val

main = let a = parseSgfToken "(;A[] ;WW[hgoehoge](;B[])(;CC[]))"
       in parseSgf a
