{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-driven operations on typeful heterogeneous lists.

-}


module HTypeDriven where

import FakePrelude
import HType
import HList
import HArray


{-----------------------------------------------------------------------------}

-- Convenience notation

infix 1 @@
(@@)  :: HUpdate l e => l -> e -> l 
(@@)  =  hUpdate


{-----------------------------------------------------------------------------}

-- Zero or more occurrences

class HOccursMany e l
 where
  hOccursMany :: l -> [e]

instance HOccursMany e HNil
 where
  hOccursMany HNil = []

instance ( HOccursMany e l, HList l )
      =>   HOccursMany e (HCons e l)
 where
  hOccursMany (HCons e l) = e:hOccursMany l

instance ( HOccursMany e l, HList l )
      =>   HOccursMany e (HCons e' l)
 where
  hOccursMany (HCons _ l) = hOccursMany l


{-----------------------------------------------------------------------------}

-- One or more occurrences

class HOccursMany1 e l
 where
  hOccursMany1 :: l -> (e,[e])

instance ( HOccursMany e l, HList l )
      =>   HOccursMany1 e (HCons e l)
 where
  hOccursMany1 (HCons e l) = (e,hOccursMany l)

instance ( HOccursMany1 e l, HList l )
      => HOccursMany1 e (HCons e' l)
 where
  hOccursMany1 (HCons _ l) = hOccursMany1 l


{-----------------------------------------------------------------------------}

-- The first occurrence

class HOccursFst e l
 where
  hOccursFst :: l -> e

instance HList l
      => HOccursFst e (HCons e l)
 where
  hOccursFst (HCons e l) = e

instance ( HOccursFst e l, HList l )
      =>   HOccursFst e (HCons e' l)
 where
  hOccursFst (HCons _ l) = hOccursFst l


{-----------------------------------------------------------------------------}

-- One occurrence and nothing is left

class HOccurs e l
 where
  hOccurs :: l -> e

instance HFreeType e l =>
         HOccurs e (HCons e (HCons x l)) where
   hOccurs (HCons e _) = e

instance (HOccurs e l) => HOccurs e (HCons e' l) where
   hOccurs (HCons _ l) = hOccurs l

instance (Force e e') => HOccurs e' (HCons e HNil) where
   hOccurs (HCons e _) = force e



{-----------------------------------------------------------------------------}

-- One occurrence and nothing is left
-- A variation that does not handle forgotten types

class HOccurs' e l
 where
  hOccurs' :: l -> e

instance ( HList l
         , HFreeType e l
         )
           => HOccurs' e (HCons e l)
 where
  hOccurs' (HCons e _) = e

instance ( HOccurs' e l
         , HList l
         )
           => HOccurs' e (HCons e' l)
 where
  hOccurs' (HCons _ l) = hOccurs' l


{-----------------------------------------------------------------------------}

-- One occurrence and nothing is left
-- A variation that avoids overlapping instances

class HOccurs'' e l
 where
  hOccurs'' :: l -> e

instance ( TypeEqBool e e' b
         , HOccursBool b e (HCons e' l) )
      =>   HOccurs'' e (HCons e' l)
 where
  hOccurs'' (HCons e' l) = e
   where
    e = hOccursBool b (HCons e' l)
    b = typeEqBool (hProxy e) (hProxy e')

class HOccursBool b e l
 where
  hOccursBool :: b -> l -> e

instance ( HList l
         , HFreeType e l
         )
           => HOccursBool HTrue e (HCons e l)
 where
  hOccursBool _ (HCons e _) = e

instance ( HOccurs'' e l
         , HList l
         )
           => HOccursBool HFalse e (HCons e' l)
 where
  hOccursBool _ (HCons _ l) = hOccurs'' l


{-----------------------------------------------------------------------------}


class HOccurs''' e l where
   hOccurs''' :: l -> e
instance HOccursWith Id e l => HOccurs''' e l where
   hOccurs''' l = hOccursWith Id l

class HOccursWith t e l where
   hOccursWith :: t -> l -> e
instance HApply t e e' => HOccursWith t e' (HCons e HNil) where
   hOccursWith t (HCons e _) = hApply t e
instance (HList (HCons x l) , HFreeType e (HCons x l)) => HOccursWith t e (HCons e (HCons x l)) where
   hOccursWith _ (HCons e _) = e
instance (HOccursWith t e l,HList l) => HOccursWith t e (HCons e' l) where
   hOccursWith t (HCons _ l) = hOccursWith t l


{-----------------------------------------------------------------------------}

-- Zero or at least one occurrence

class HOccursOpt e l
 where
  hOccursOpt :: l -> Maybe e

instance HOccursOpt e HNil
 where
  hOccursOpt HNil = Nothing

instance HOccursOpt e (HCons e l)
 where
  hOccursOpt (HCons e l) = Just e

instance HOccursOpt e l
      => HOccursOpt e (HCons e' l)
 where
  hOccursOpt (HCons _ l) = hOccursOpt l


{-----------------------------------------------------------------------------}

-- Class to test that a type is "free" in a type sequence

class HFreeType e l
instance HFreeType e HNil
instance (TypeNotEq e e', HFreeType e l)
      =>  HFreeType e (HCons e' l)


{-----------------------------------------------------------------------------}

-- Map a type to a natural

class HNat n => HType2HNat l e n | l e -> n
 where
  hType2HNat :: l -> HProxy e -> n

instance ( TypeEqBool e' e b
         , HType2HNat' b l e n
         )
           => HType2HNat (HCons e' l) e n
 where
  hType2HNat (HCons e' l) p = n
   where
    b = typeEqBool (hProxy e') p
    n = hType2HNat' b l p 


-- Helper class

class (HBool b, HNat n) => HType2HNat' b l e n | b l e -> n
 where
  hType2HNat' :: b -> l -> HProxy e -> n

instance HFreeType e l
      => HType2HNat' HTrue l e HZero
 where
  hType2HNat' _ _ _ = HZero

instance HType2HNat l e n
      => HType2HNat' HFalse l e (HSucc n)
 where
  hType2HNat' _ l p = HSucc (hType2HNat l p)


{-----------------------------------------------------------------------------}

-- Define type-index look-up in terms of the natural-based primitive

instance ( HExtend h t l
         , HType2HNat l e' i
         , HLookup l i e
         )
           => HLookup (HCons h t) (HProxy e') e
 where
  hLookup (HCons h t) p = hLookup l (hType2HNat l p)
   where
    l = hExtend h t


{-----------------------------------------------------------------------------}

-- Define type-index delete in terms of the natural-based primitive

instance ( HExtend h t l
         , HType2HNat l e i
         , HDelete l i l'
         )
           => HDelete (HCons h t) (HProxy e) l'
 where
  hDelete (HCons h t) p = hDelete l (hType2HNat l p)
   where
    l = hExtend h t


{-----------------------------------------------------------------------------}

-- Stanamic equality

class HEqList x y
 where
  hEqList :: x -> y -> Bool

instance HEqList HNil HNil
 where
  hEqList _ _ = True

instance HEqList HNil (HCons e l)
 where
  hEqList _ _ = False

instance HEqList (HCons e l) HNil
 where
  hEqList _ _ = False

instance ( TypeEqBool e e' b
         , HEqList l l'
         , HEqList' b e e'
         )
      =>   HEqList (HCons e l) (HCons e' l')
 where
  hEqList (HCons e l) (HCons e' l') = (hEqList' b e e') && b'
   where
    b  = typeEqBool (hProxy e) (hProxy e')
    b' = hEqList l l'

class HEqList' b e e'
 where
  hEqList' :: b -> e -> e' -> Bool

instance HEqList' HFalse e e'
 where
  hEqList' _ _ _ = False

instance Eq e => HEqList' HTrue e e
 where
  hEqList' _ = (==)


{-----------------------------------------------------------------------------}

-- Define type-index update in terms of the natural-based primitives

instance ( HType2HNat l e i
         , HUpdateTP l i e
         )
           => HUpdateTP l (HProxy e) e
 where
  hUpdateTP l p e = hUpdateTP l (hType2HNat l p) e 


instance ( HType2HNat l e i
         , HUpdateTC l i e l'
         )
           => HUpdateTC l (HProxy e) e l'
 where
  hUpdateTC l p e = hUpdateTC l (hType2HNat l p) e 


{-----------------------------------------------------------------------------}

-- Type-indexed update

class HUpdate l e
 where
  hUpdate :: l -> e -> l

instance HUpdateTP (HCons e' l) (HProxy e) e
      => HUpdate (HCons e' l) e
 where
  hUpdate l e = hUpdateTP l (hProxy e) e



{-----------------------------------------------------------------------------}

{-

*HTypeDriven> hOccurs myAnimal :: Breed
Cow

*HTypeDriven> hProject myAnimal (HCons (HProxy::HProxy Breed) HNil)
HCons Cow HNil

*HTypeDriven> fst (hSplit myAnimal (HCons (HProxy::HProxy Breed) HNil))
HCons Cow HNil

-}

{-----------------------------------------------------------------------------}
