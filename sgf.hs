import SgfParser
import SgfIgoParser
import Data.List (intercalate, sort)
import System.Environment(getArgs)

data Hoge a = Hoge {
  h_dx   :: Int,
  h_dy   :: Int,
  h_node :: [SProp a]
} deriving (Show)

instance Eq (Hoge a) where
  a == b = (h_dx a) == (h_dx b) && (h_dy a) == (h_dy b)
  

instance Ord (Hoge a) where
  compare a b = compare (h_dx a) (h_dx b)

-- UGLY CODE!!
instance (Show a) => Show (SgfTreeNode a) where
  show tn = show_matrix $ make_matrix tn
    where
      make_matrix tn = helper1 0 0 tn
      helper1 dx dy (SgfTreeNode n ts) = let tmp = helper2 dx (dy + 1) ts
                                         in  Hoge dx dy n : (concat tmp)
      helper2 _  _  []     = []
      helper2 dx dy (t:ts) = let z = helper1 dx dy t
                             in (:) z $ helper2 (max_dx z + 1) dy ts
      max_dx z = foldr max 0 $ map h_dx z
      show_matrix hoges = helper3 0 hoges
      helper3 d hoges = let filtered1 = sort $ filter (\x -> h_dy x == d) hoges in
                        let filtered2 = map h_dx (filter (\x -> h_dy x == (d+1)) hoges)
                        in if null filtered1 then "" else 
                           show_filtered filtered1 filtered2 ++ "\n" ++ (helper3 (d + 1) hoges)
      show_snode ps = intercalate "&" $ map show_sprop ps
      show_filtered filtered1 filtered2 = helper4 0 filtered1 filtered2
      helper4 _ [] _   = ""
      helper4 d (f:[]) filtered2 = let z = foldr max 0 filtered2 in
                                   if d < 10 * h_dx f then " " ++ helper4 (d + 1) [f] filtered2
                                   else let str = (show_snode . h_node $ f) in
                                     str ++ (helper5 ((z - h_dx f) * 10 - length str))
      helper4 d (f1:f2:fs) filtered2 = if d < 10 * h_dx f1 then " " ++ helper4 (d + 1) (f1:(f2:fs)) filtered2
                                       else let str = show_snode . h_node $ f1 in
                                       let z = foldr max 0 $ filter (<(h_dx f2)) filtered2 in
                                       let y = (z - h_dx f1) * 10 - length str
                                       in str ++ (helper5 y) ++ helper4 (d + (if y > 0 then y else 0) + length str) (f2:fs) filtered2
      helper5 n
        | n <= 0    = ""
        | otherwise = take n $ repeat '-'

show_sprop p = (s_ident p) ++ "[" ++ show (s_blocks p) ++ "]"

instance Show IgoMove where
  show (IgoMove (x, y)) = x:y:""

instance (Show p, Show m, Show s) => Show (ValueType p m s) where
  show VNone            = ""
  show (VNumber x)      = show x
  show (VReal x)        = show x
  show (VDouble x)      = show x
  show (VColor  x)      = show x
  show (VText   x)      = take 7 x ++ ".."
  show (VSimpleText x)  = take 7 x ++ ".."
  show (VPoint x)       = show x
  show (VMove x)        = show x
  show (VStone x)       = show x
  show (VList x)        = show x
  show (VPair x)        = show x
  show (VParseError x)  = "!P"
  show (VUnknownProp x) = "!U"

main = do fname <- fmap head getArgs
          inpStr <- readFile fname
          case parseSgf igo_dict inpStr of
            Left  err -> putStrLn $ show err
            Right val -> putStrLn $ show val
    
