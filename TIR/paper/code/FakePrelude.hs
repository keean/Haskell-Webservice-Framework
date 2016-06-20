{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Some very basic technology for faking dependent types in Haskell.
   Namely: type-level Booleans, naturals, ordering.

 -}

  
module FakePrelude where


{-----------------------------------------------------------------------------}

-- Type-level Booleans

data HTrue  = HTrue  deriving Show
data HFalse = HFalse deriving Show
 
class HBool x
instance HBool HTrue
instance HBool HFalse


-- Typical operations on Booleans

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


{-----------------------------------------------------------------------------}

-- Type-level maybies

data HNothing  = HNothing  deriving Show
data HJust x   = HJust x   deriving Show


{-----------------------------------------------------------------------------}

-- Type-level naturals

data HZero   = HZero    deriving Show
data HSucc n = HSucc n  deriving Show

class HNat n
 where
  hNat2Integral :: Integral i => n -> i
 
instance HNat HZero
 where
  hNat2Integral _ = 0
 
instance HNat n => HNat (HSucc n)
 where
  hNat2Integral (HSucc n) = hNat2Integral n + 1


-- An order on naturals
 
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


{-----------------------------------------------------------------------------}

-- Ordering and equality lifted to the type level

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


{-----------------------------------------------------------------------------}
