{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module SingletonHOccurs where

import MiniHList

-- omitted for the sake of hugs
-- import HType

-- Could be used insted of Force
class Cast x y | x -> y
 where
  cast :: x -> y

class HOccurs e l where
   hOccurs :: l -> e

instance -- HFreeType e l =>
         -- omitted for the sake of hugs
         HOccurs e (HCons e (HCons x l)) where
   hOccurs (HCons e _) = e

instance (HOccurs e l) => HOccurs e (HCons e' l) where
   hOccurs (HCons _ l) = hOccurs l

instance (Cast e e') => HOccurs e' (HCons e HNil) where
   hOccurs (HCons e _) = cast  e

{-----------------------------------------------------------------------------}

-- Class to test that a type is "free" in a type sequence

{-

omitted for the sake of hugs

class HFreeType e l
instance HFreeType e HNil
instance (TypeNotEq e e', HFreeType e l)
      =>  HFreeType e (HCons e' l)
-}


{-

`Too polymorphic'

instance HOccurs e (HCons e HNil) where
   hOccurs (HCons e _) = e

    No instance for (HOccurs e (HCons Bool HNil))
      arising from use of `hOccurs' at <interactive>:1
    In the definition of `it': it = hOccurs (HCons True HNil)

-}
