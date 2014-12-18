module SgfIgoParser (igo_dict) where

import SgfParser
import Text.ParserCombinators.Parsec (letter)
import Control.Applicative ((<*>), (<$>))

type IgoType = ValueType (Char, Char) (Char, Char) (Char, Char)

igo_dict :: [(String, [String] -> Maybe IgoType)]
igo_dict = base ++ igo
  where
    base = base_dict point_parser move_parser stone_parser
    point_parser = fmap VPoint $ (,) <$> letter <*> letter
    move_parser  = fmap VMove  $ (,) <$> letter <*> letter
    stone_parser = fmap VStone $ (,) <$> letter <*> letter
    igo = [ ("HA", just_one number_parser),
            ("KM", just_one real_parser),
            ("TB", point_elist_of point_parser),
            ("TW", point_elist_of point_parser) ]
