module Lib.Data.Version (Version(..)) where

import Numeric

data Version = Version [Int]

instance Eq Version where
    Version [] == Version [] = True
    Version [] == Version _  = False
    Version _ == Version []  = False
    Version (a0:as) == Version (b0:bs) = if a0 == b0
        then Version as == Version bs
        else False

instance Ord Version where
    compare (Version []) (Version []) = EQ
    compare _ (Version []) = GT
    compare (Version []) _ = LT
    compare (Version (a0:as)) (Version (b0:bs))
        | a0 > b0 = GT
        | a0 < b0 = LT
        | otherwise = compare (Version as) (Version bs)

instance Show Version where
    showsPrec _ (Version []) = id
    showsPrec _ (Version (v0:[])) = showInt v0
    showsPrec _ (Version (v0:vs)) = showInt v0 . showChar '.' . shows (Version vs)
