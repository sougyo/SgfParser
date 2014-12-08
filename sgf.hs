import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Text.Parsec.Prim (tokenPrim, getPosition)
import Text.Parsec.Pos (SourcePos)
import Data.List

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

data SValueType = SNone           |
                  SNumber Integer |
                  SText String    |
                  SPoint Int Int
  deriving (Show, Eq)

-- A[111][222]
data SProp a = SProp {
  s_ident  :: String,
  s_blocks :: [a]
} deriving (Show)

-- A[111][222]B[333]
data SNode a = SNode {
  s_props :: [SProp a]
} deriving (Show)

-- (A[111][222]B[333]) children
data SgfTreeNode a = SgfTreeNode {
  s_node     :: (SNode a),
  s_children ::  [SgfTreeNode a]
}

block_map f (SgfTreeNode n ts) = SgfTreeNode (node_map f n) $ map (block_map f) ts
  where
    node_map f (SNode props) = SNode $ map (prop_map f) props
    prop_map f (SProp i xs)  = SProp i $ map f xs


data Hoge a = Hoge {
  h_dx   :: Int,
  h_dy   :: Int,
  h_node :: SNode a
} deriving (Show)

instance (Show a) => Show (SgfTreeNode a) where
  show tn = show_matrix $ make_matrix tn
    where
      make_matrix tn = helper1 0 0 tn
      helper1 dx dy (SgfTreeNode n ts) = (:) (Hoge dx dy n) $ concat $ helper2 dx (dy + 1) ts
      helper2 _  _  []     = []
      helper2 dx dy (t:ts) = let z = helper1 dx dy t
                             in (:) z $ helper2 ((max_dx z) + 1) dy ts
      max_dx z = foldr max 0 (map h_dx z)
      show_matrix hoges = helper3 0 hoges
      helper3 d hoges = let filtered = filter (\x -> h_dy x == d) hoges
                        in if null filtered then "" else 
                           show_filtered filtered ++ "\n" ++ (helper3 (d + 1) hoges)
      show_snode (SNode ps) = intercalate ":" $ map (show . s_ident) ps
      show_filtered filtered = helper4 0 filtered
      helper4 _ []     = ""
      helper4 d (f:fs) = if d < (10 * h_dx f) then " " ++ helper4 (d + 1) (f:fs)
                         else let str = (show_snode . h_node) f
                              in str ++ helper4 (d + length str) fs

sgfParser = SgfTreeNode (SNode []) <$> many1 gameTree
  where
    gameTree     = do token LeftParenthes
                      ns <- many1 node
                      ts <- many gameTree
                      token RightParenthes
                      return $ makeGameTree ns ts
    node         = token Semicolon *> (fmap SNode $ many property)
    property     = SProp <$> propIdent <*> many1 propValue
    propIdent    = fmap ucword2s $ tSatisfy isUcWord
    propValue    = fmap block2s $ tSatisfy isBracketBlock
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
  Right val -> putStrLn $ show $ block_map (\x -> SNone) val

main = parseSgf $ parseSgfToken "(;MN[333]A[0];HO[234];MI[aaa](;B[33])(;Z[344];X[324]))(;NN[234])"
