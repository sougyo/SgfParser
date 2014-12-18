import SgfParser
import SgfIgoParser
import Data.List (intercalate)

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
      show_snode ps = intercalate ":" $ map show ps
      show_filtered filtered = helper4 0 filtered
      helper4 _ []     = ""
      helper4 d (f:fs) = if d < 10 * h_dx f then " " ++ helper4 (d + 1) (f:fs)
                         else let str = show_snode . h_node $ f
                              in str ++ helper4 (d + length str) fs


main = parseSgf igo_dict "(;HA[33333];TB[cc][ac:aa];LB[cc:a\\:a];GC[aa:\na];ON[aa\na];CR[cc][aa:aa][aa];SZ[+33333:-32494329](;W[aa])(;US[hoge\nho\tge]))"
