{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
 
module HBool where

data HTrue  = HTrue  deriving Show
data HFalse = HFalse deriving Show

class HBool x
instance HBool HTrue
instance HBool HFalse

class (HBool t, HBool t') => HNot t t' | t -> t'
 where
  hNot :: t -> t'

instance HNot HTrue HFalse
 where
  hNot = const HFalse

instance HNot HFalse HTrue
 where
  hNot = const HTrue

class (HBool t, HBool t', HBool t'') => HOr t t' t'' | t t' -> t''
 where
  hOr :: t -> t' -> t''

instance HOr HFalse HFalse HFalse
 where
  hOr _ _ = HFalse

instance HOr HTrue HFalse HTrue
 where
  hOr _ _ = HTrue

instance HOr HFalse HTrue HTrue
 where
  hOr _ _ = HTrue

instance HOr HTrue HTrue HTrue
 where
  hOr _ _ = HTrue

class HBool t => HCond t x y z | t x y -> z
 where
  hCond :: t -> x -> y -> z

instance HCond HFalse x y y
 where
  hCond _ _ y = y

instance HCond HTrue x y x
 where
  hCond _ x _ = x
