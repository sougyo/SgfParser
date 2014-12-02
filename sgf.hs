import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)

data MToken = LeftParenthes  |
              RightParenthes |
              LeftBracket    |
              RightBracket   |
              Semicolon      |
              Ident String
  deriving (Show, Eq)

data MNode = MNode {
  m_position :: SourcePos,
  m_token    :: MToken
} deriving (Show, Eq)

hoge = terminate '(' LeftParenthes   <|>
       terminate ')' RightParenthes  <|>
       terminate '[' LeftBracket     <|>
       terminate ']' RightBracket    <|>
       terminate ';' Semicolon       <|>
       (many1 alphaNum >>= \s -> return $ Ident s)
  where terminate c m = try $ char c *> return m

hoge2 = do p <- getPosition
           t <- hoge
           return $ MNode p t

sgfTokenParser = many1 $ spaces *> hoge2

sgfParser = do mySatisfy $ \x -> (m_token x) == LeftParenthes
               mySatisfy $ \x -> (m_token x) == Ident "hoge"
               mySatisfy $ \x -> (m_token x) == RightParenthes

parseSgfToken input = case parse sgfTokenParser "" input of
     Left err  -> []
     Right val -> val

mySatisfy f =
  tokenPrim (\c -> show c)
  (\pos c _cs -> m_position c)
  (\c -> if f c then Just c else Nothing)
              
parseSgf input = case parse sgfParser "" input of
  Left  err -> putStrLn $ show err
  Right val -> putStrLn $ show val


main = let a = parseSgfToken "(hoge)"
       in parseSgf a
