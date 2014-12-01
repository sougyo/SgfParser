import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim)
import Text.Parsec.Pos (initialPos)

data MTokens = LeftParenthes  |
               RightParenthes |
               LeftBracket    |
               RightBracket   |
               Semicolon      |
               Ident String
  deriving (Show, Eq)

hoge = gen '(' LeftParenthes   <|>
       gen ')' RightParenthes  <|>
       gen '[' LeftBracket     <|>
       gen ']' RightBracket    <|>
       gen ';' Semicolon       <|>
       (many1 alphaNum >>= \s -> return $ Ident s)
  where gen c m = try (char c *> return m)

sgfTokenParser = many1 (spaces *> hoge)

sgfParser = mySatisfy (\x -> x == LeftParenthes)

parseSgfToken input = case parse sgfTokenParser "" input of
     Left err  -> []
     Right val -> val

mySatisfy f =
  tokenPrim (\c -> show c)
  (\pos c _cs -> pos)
  (\c -> if f c then Just c else Nothing)
              
parseSgf input = case parse sgfParser "" input of
  Left  err -> putStrLn $ show err
  Right val -> putStrLn $ show val


main = let a = parseSgfToken "(hoge)"
       in parseSgf a
