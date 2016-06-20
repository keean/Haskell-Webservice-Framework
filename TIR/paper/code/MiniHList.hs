{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Typeful heterogeneous lists.

 -}

 
module MiniHList where

import Data.Typeable
import FakePrelude


{-----------------------------------------------------------------------------}

-- Heterogeneous type sequences

data HNil      = HNil      deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)


{-----------------------------------------------------------------------------}

-- The set of all types of heterogeneous lists

class HList l
instance HList HNil
instance HList l => HList (HCons e l)


{-----------------------------------------------------------------------------}

-- Public constructor

hNil  :: HNil
hNil  =  HNil

hCons :: HList l => e -> l -> HCons e l
hCons e l = HCons e l


{-----------------------------------------------------------------------------}

-- Basic list functions

class HHead l h | l -> h
 where
  hHead :: l -> h

instance HHead (HCons e l) e
 where
  hHead (HCons e _) = e

class HTail l l' | l -> l'
 where
  hTail :: l -> l'

instance HTail (HCons e l) l
 where
  hTail (HCons _ l) = l



{-----------------------------------------------------------------------------}

-- A class for extension

class HExtend e l l' | e l -> l'
 where
  hExtend :: e -> l -> l'

instance HExtend e HNil (HCons e HNil)
 where
  hExtend e l = HCons e l

instance HList l => HExtend e (HCons e' l) (HCons e (HCons e' l))
 where
  hExtend e l = HCons e l


{-----------------------------------------------------------------------------}

-- Appending HLists

-- The normal append for comparison

append :: [a] -> [a] -> [a]
append [] l = l
append (x:l) l' = x : append l l'


-- The class HAppend

class HAppend l l' l'' | l l' -> l''
 where
  hAppend :: l -> l' -> l''


-- The instance following the normal append

instance HList l => HAppend HNil l l
 where
  hAppend HNil l = l

instance (HAppend l l' l'', HList l'')
      => HAppend (HCons x l) l' (HCons x l'')
 where
  hAppend (HCons x l) l' = hCons x (hAppend l l')

   
{-----------------------------------------------------------------------------}

-- Reversing HLists

-- The class HReverse
-- Naive; could use standard higher-order trick.

class HReverse l l' | l -> l'
 where
  hReverse :: l -> l'

instance HReverse HNil HNil
 where
  hReverse HNil = HNil

instance ( HReverse l l'
         , HAppend l' (HCons e HNil) l''
         )
      =>   HReverse (HCons e l) l''
 where
  hReverse (HCons e l) = hAppend (hReverse l) (HCons e HNil)


{-----------------------------------------------------------------------------}

-- Qualification of lists

class HQualify u a q | u a -> q, q a -> u
 where
  hQualify   :: u -> a -> q
  hUnqualify :: q -> a -> u

instance HQualify HNil a HNil
 where
  hQualify   HNil _ = hNil
  hUnqualify HNil _ = hNil 

instance (HQualify l a l', HList l, HList l') 
      => HQualify (HCons e l) a (HCons (e,a) l')
 where
  hQualify   (HCons e l) a     = hCons (e,a) (hQualify l a)
  hUnqualify (HCons (e,_) l) a = hCons e     (hUnqualify l a)


{-----------------------------------------------------------------------------}

-- A heterogeneous apply operator

class HApply f a r | f a -> r
 where
  hApply :: f -> a -> r


-- Normal function application

instance HApply (x -> y) x y
 where
  hApply f x = f x


{-----------------------------------------------------------------------------}

-- A heterogeneous fold for all types

class HList l => HFoldr f v l r | f v l -> r
 where
  hFoldr :: f -> v -> l -> r

instance HFoldr f v HNil v
 where
  hFoldr _ v _ = v

instance ( HFoldr f v l r
         , HApply f (e,r) r'
         )
      => HFoldr f v (HCons e l) r'
 where
  hFoldr f v (HCons e l) = hApply f (e,hFoldr f v l)


{-----------------------------------------------------------------------------}

-- Map a heterogeneous list to a homogeneous one

class HMapOut f r e
 where
  hMapOut :: f -> r -> [e]

instance HMapOut f HNil e
 where
  hMapOut _ _ = []

instance ( HMapOut f l e'
         , HApply f e e'
         )
      =>   HMapOut f (HCons e l) e'
 where
  hMapOut f (HCons e l) = hApply f e : hMapOut f l


{-----------------------------------------------------------------------------}

-- A reconstruction of append

append' :: [a] -> [a] -> [a]
append' l l' = foldr (:) l' l

hAppend' l l' = hFoldr ApplyHCons l' l

data ApplyHCons = ApplyHCons

instance HList l => HApply ApplyHCons (e,l) (HCons e l)
 where
  hApply ApplyHCons (e,l) = hCons e l


{-----------------------------------------------------------------------------}

-- A heterogeneous map for all types

class HList l => HMap f l l' | f l -> l'
 where
  hmap :: f -> l -> l'

instance HMap f HNil HNil
 where
  hmap _ HNil = hNil

instance ( HApply f e e'
         , HMap f l l'
         , HList l
         , HList l'
         )
      => HMap f (HCons e l) (HCons e' l')
 where
  hmap f (HCons e l) = hCons (hApply f e) (hmap f l)


{-----------------------------------------------------------------------------}

-- A function for showing

data HShow  = HShow
data HSeq x = HSeq x

instance (Typeable x, Show x)
      => HApply HShow x (IO ())
 where
  hApply _ x = do putStrLn $    show (typeOf x)
                             ++ " -> " 
                             ++ show x

instance ( Monad m 
         , HApply f x (m ())
         )
      => HApply (HSeq f) (x,m ()) (m ())
 where
  hApply (HSeq f) (x,c) = do hApply f x; c


{-----------------------------------------------------------------------------}
