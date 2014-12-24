module SgfIgoParser (igo_dict, IgoMove(..)) where

import SgfParser
import Text.ParserCombinators.Parsec (letter)
import Control.Applicative ((<*>), (<$>))

newtype IgoMove = IgoMove (Char, Char) deriving (Eq)
type IgoType = ValueType IgoMove IgoMove IgoMove

igo_dict :: [(String, [String] -> Maybe IgoType)]
igo_dict = base ++ igo_props
  where
    base = base_dict point_parser move_parser stone_parser
    point_parser = VPoint <$> igo_move_parser
    move_parser  = VMove  <$> igo_move_parser
    stone_parser = VStone <$> igo_move_parser
    igo_move_parser = fmap IgoMove $ (,) <$> letter <*> letter
    igo_props = [
      ("HA", just_one number_parser),
      ("KM", just_one real_parser),
      ("TB", point_elist_of point_parser),
      ("TW", point_elist_of point_parser) ]
