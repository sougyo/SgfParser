import SgfParser
import SgfIgoParser
import Data.List (intercalate, sort, sortBy)
import System.Environment(getArgs)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad (when)

data Cell a = Cell {
  c_x    :: Int,
  c_y    :: Int,
  h_node :: [SProp a]
} deriving (Show)

sortBy_x = sortBy $ \a b -> compare (c_x a) (c_x b)

make_matrix t = process_subtree 0 0 t
  where
    process_subtree x y t = let rest = concat $ process_children x (y + 1) (s_children t)
                            in Cell x y (s_node t) : rest 
    process_children _ _ []     = []
    process_children x y (t:ts) = let cs = process_subtree x y t
                                  in (:) cs $ process_children (max_x cs + 1) y ts
    max_x cs = foldr max 0 $ map c_x cs
   
show_matrix cs = execWriter $ do next_line 0 cs
  where
    next_line l cs = do let f1 = sortBy_x $ filter (\x -> c_y x == l)     cs
                        let f2 = map c_x  $ filter (\x -> c_y x == l + 1) cs
                        when (not $ null f1) $ do trace_line 0 f1 f2
                                                  tell "\n"
                                                  next_line (l + 1) cs
    trace_line _   []         _  = return ()
    trace_line col ess@(e:es) f2 = do
        s_len <- padding_space $ width * (c_x e) - col
        n_len <- tell_node e
        h_len <- write_edges (col + s_len + n_len) $ sort $ filter (children_filter ess) f2
        trace_line (col + s_len + n_len + h_len) es f2
    padding_space n
      | n <= 0    = return 0
      | otherwise = do tell $ take n $ repeat ' '
                       return n
    children_filter (e:es) x = c_x e < x && (null es || x < (c_x $ head es))
    tell_node n = let str = show_snode . h_node $ n
                  in tell str >> (return $ length str)
    write_edges _ []     = return 0
    write_edges c (x:xs) = do e_len <- write_edge $ x * width - c
                              c_len <- write_edges (c + e_len) xs
                              return $ e_len + c_len
    write_edge n
        | n <= 0    = return 0
        | otherwise = do tell $ take (n + 2) $ repeat '-'
                         tell "+"
                         return $ n + 3
    width = 10

instance (Show a) => Show (SgfTreeNode a) where
  show tn = show_matrix $ make_matrix tn

show_snode ps = intercalate "&" $ map show_sprop ps
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
    
