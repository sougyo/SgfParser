module SgfUtil (show) where

import SgfParser
import Data.List (intercalate, sort, sortBy)
import Control.Monad.Trans.Writer.Lazy
import Control.Monad (when)

data Cell a = Cell {
  c_x    :: Int,
  c_y    :: Int,
  c_node :: [SgfProperty a]
} deriving (Show)

make_matrix t = process_subtree 0 0 t
  where
    process_subtree x y t = let rest = concat $ process_children x (y + 1) (sgf_children t)
                            in Cell x y (sgf_node t) : rest 
    process_children _ _ []     = []
    process_children x y (t:ts) = let cs = process_subtree x y t
                                  in (:) cs $ process_children (max_x cs + 1) y ts
    max_x cs = foldr max 0 $ map c_x cs
   
show_matrix cs = execWriter $ do next_line 0 cs
  where
    next_line l cs = do let line_cells      = filter (\x -> c_y x == l)     cs
                        let next_line_cells = filter (\x -> c_y x == l + 1) cs
                        when (not $ null line_cells) $ do
                          trace_line 0 (sortBy_x line_cells) (map c_x next_line_cells)
                          tell "\n"
                          next_line (l + 1) cs
    trace_line _   []         _  = return ()
    trace_line col css@(c:cs) xs = do
        s_len <- write_space $ width * (c_x c) - col
        n_len <- write_node c
        e_len <- write_edges (col + s_len + n_len) $ sort $ filter (childrens_x_filter css) xs
        trace_line (col + s_len + n_len + e_len) cs xs
    write_space n
      | n <= 0    = return 0
      | otherwise = do tell $ take n $ repeat ' '
                       return n
    childrens_x_filter (c:cs) x = c_x c < x && (null cs || x < (c_x $ head cs))
    write_node n = let str = show_snode . c_node $ n
                   in tell str >> (return $ length str)
    write_edges _ []     = return 0
    write_edges c (x:xs) = do e_len <- write_edge $ x * width - c
                              r_len <- write_edges (c + e_len) xs
                              return $ e_len + r_len
    write_edge n
        | n <= 0    = return 0
        | otherwise = do tell $ take (n + 2) $ repeat '-'
                         tell "+"
                         return $ n + 3
    width = 10
    sortBy_x = sortBy $ \a b -> compare (c_x a) (c_x b)
    show_snode ps = intercalate "&" $ map show_sprop ps
    show_sprop p = (sgf_ident p) ++ "[" ++ show (sgf_blocks p) ++ "]"

instance (Show a) => Show (SgfTreeNode a) where
  show tn = show_matrix $ make_matrix tn

instance Show IgoMoveType where
  show (IgoMoveType (x, y)) = x:y:""

instance (Show p, Show m, Show s) => Show (SgfValueType p m s) where
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
  show (VComposition x) = show x
  show (VParseError x)  = "*ParseError*"
  show (VUnknownProp x) = "*UnknownProp*"
