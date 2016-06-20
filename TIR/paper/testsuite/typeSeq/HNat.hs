{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
 
module HNat where

import HOrd

data HZero = HZero deriving Show
data HNat n => HSucc n = HSucc n deriving Show

class HNat n
 where
  hNat2Integral :: Integral i => n -> i

instance HNat HZero
 where
  hNat2Integral _ = 0

instance HNat n => HNat (HSucc n)
 where
  hNat2Integral (HSucc n) = hNat2Integral n + 1

instance HOrd HZero HZero HEq
 where
  hOrd _ _ = HEq

instance (HNat x) => HOrd HZero (HSucc x) HLt
 where
  hOrd _ _ = HLt

instance HOrd (HSucc x) HZero HGt
 where
  hOrd _ _ = HGt

instance (HNat x, HNat y, HOrd x y o) => HOrd (HSucc x) (HSucc y) o 
 where
  hOrd (HSucc x) (HSucc y) = hOrd x y

