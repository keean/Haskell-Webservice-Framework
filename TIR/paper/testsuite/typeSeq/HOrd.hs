{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module HOrd where

import HBool

class HOrd x y o | x y -> o
 where
  hOrd :: x -> y -> o

class HLtBool x y b | x y -> b
 where
  hLtBool :: x -> y -> b

class HEqBool x y b | x y -> b
 where
  hEqBool :: x -> y -> b

class HGtBool x y b | x y -> b
 where
  hGtBool :: x -> y -> b

data HLt = HLt deriving Show
data HEq = HEq deriving Show
data HGt = HGt deriving Show
data HNoOrdering = HNoOrdering deriving Show

instance ( HOrd x y o
         , HOrd2Bool HEq o b
         ) 
           => HEqBool x y b
 where
  hEqBool x y = hOrd2Bool HEq (hOrd x y)

class HOrd2Bool o o' b | o o' -> b
 where
  hOrd2Bool :: o -> o' -> b

instance HOrd2Bool HLt HLt HTrue
 where
  hOrd2Bool _ _ = HTrue

instance HOrd2Bool HEq HEq HTrue
 where
  hOrd2Bool _ _ = HTrue

instance HOrd2Bool HGt HGt HTrue
 where
  hOrd2Bool _ _ = HTrue

instance HOrd2Bool HLt HEq HFalse
 where
  hOrd2Bool _ _ = HFalse

instance HOrd2Bool HLt HGt HFalse
 where
  hOrd2Bool _ _ = HFalse

instance HOrd2Bool HEq HLt HFalse
 where
  hOrd2Bool _ _ = HFalse

instance HOrd2Bool HEq HGt HFalse
 where
  hOrd2Bool _ _ = HFalse

instance HOrd2Bool HGt HLt HFalse
 where
  hOrd2Bool _ _ = HFalse

instance HOrd2Bool HGt HEq HFalse
 where
  hOrd2Bool _ _ = HFalse

