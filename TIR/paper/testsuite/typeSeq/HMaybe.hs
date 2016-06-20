{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
 
module HMaybe where

data HNothing = HNothing deriving Show
data HJust x = HJust x deriving Show
