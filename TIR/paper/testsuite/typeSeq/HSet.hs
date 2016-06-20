{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}


{-

THIS MODULE NEEDS WORK.
SUBSUMED MORE OR LESS BY THE TI* MODULES.
SUBSET ISSUE NEEDS TO BE MADE AVAILABLE HERE OR ELSEWHERE.
NO CLAIMS FOR NOW.

-}

 
module HSet where

import HType
import HBool
import HList

class Member e s
instance Member e (HCons e s)
instance Member e s => Member e (HCons e' s)

class NotMember e s
instance NotMember e HNil
instance (NotMember e s, TypeNotEq e e') => NotMember e (HCons e' s)

class HSet s 
instance HSet HNil
instance (HSet s, NotMember e s) => HSet (HCons e s)


class (HList l, HBool t) => Member' x l t | l x -> t
 where
  member :: x -> l -> t

instance Member' x HNil HFalse
 where
  member _ _ = HFalse

instance HList l => Member' x (HCons x l) HTrue
 where
  member _ _ = HTrue

instance Member' x l t => Member' x (HCons x' l) t
 where
  member x (HCons _ l) = member x l

class (HSet s, HSet s') => Subset s s'
 where
  subset :: s -> s' -> ()
  subset _ _ = ()

class (HSet s, HSet s') => SubsetEq s s'
 where
  subseteq :: s -> s' -> ()
  subseteq _ _ = ()

class (HSet s, HSet s') => SetEq s s'
 where
  seteq :: s -> s' -> ()
  seteq _ _ = ()

instance HSet (HCons x s) => Subset HNil (HCons x s)
instance HSet s => SubsetEq HNil s
instance SetEq HNil HNil

instance ( HSet (HCons x s)
         , HSet s'
         , Member x s'
         , Subset s s'
         )
           => Subset (HCons x s) s'

instance ( HSet (HCons x s)
         , HSet s'
         , Member x s'
         , SubsetEq s s'
         )
           => SubsetEq (HCons x s) s'

instance ( SubsetEq s  s'
         , SubsetEq s' s
         )
           => SetEq s s'

class (HList s, HList s') => Sub s x s' | s x -> s'
 where
  sub :: s -> x -> s'

instance Sub HNil x HNil
 where
  sub HNil _ = HNil

instance (HList l, HList l', Sub l x l') => Sub (HCons x l) x l'
 where
  sub (HCons _ l) x = sub l x
 
instance Sub' l x l' => Sub l x l'
 where
  sub l x = sub' l x

class (HList s, HList s') => Sub' s x s' | s x -> s'
 where
  sub' :: s -> x -> s'

instance ( TypeNotEq x x'
         , Sub l x l'
         )
           => Sub' (HCons x' l) x (HCons x' l')
 where
  sub' (HCons x' l) x = HCons x' (sub l x)

class ( HSet s
      , HSet s'
      , HSet s''
      , HAppend s s' s''
      )
        => Union s s' s'' | s s' -> s''
 where
  union :: s -> s' -> s''

instance ( HAppend s s' s''
         , HSet s
         , HSet s'
         , HSet s''
         )
           => Union s s' s''
 where
  union = hAppend

class (HSet s, HSet s', HSet s'') => Difference s s' s'' | s s' -> s''
 where
  difference :: s -> s' -> s''

instance HSet s => Difference s HNil s
 where
  difference s _ = s

instance ( HSet s
         , HSet s''
         , HList s'
         , HSet (HCons x s')
         , Sub s x s'''
         , Difference s''' s' s''
         )
           => Difference s (HCons x s') s''
 where 
  difference s (HCons x s') = difference (sub s x) s'

class (HSet s, HSet s', HSet s'') => Intersection s s' s'' | s s' -> s''
 where
  intersection :: s -> s' -> s''

instance HSet s => Intersection HNil s HNil
 where
  intersection HNil _ = HNil

instance ( HSet (HCons x s)
         , HList s
         , HSet s'
         , HSet s''
         , Member' x s' b
         , HCond b HNil (HCons x HNil) x'
         , Intersection s s' s''' 
         , Union x' s''' s''
         )
           => Intersection (HCons x s) s' s''
 where
  intersection (HCons x s) s'
    = (hCond (x `member` s') HNil (HCons x HNil))
      `union`
      (s `intersection` s')



-- A version without logic programming

class (HSet s, HSet s', HSet s'') => Intersection' s s' s'' | s s' -> s''
 where
  intersection' :: s -> s' -> s''

instance HSet s => Intersection' HNil s HNil
 where
  intersection' HNil _ = HNil

instance ( HSet (HCons x s)
         , HList s
         , HSet s'
         , HSet (HCons x s'')
         , HList s''
         , Member x s'
         , Intersection' s s' s''
         )
           => Intersection' (HCons x s) s' (HCons x s'')
 where
  intersection' (HCons x s) s' = HCons x (s `intersection'` s')

{-
instance ( HSet (HCons x s)
         , HList s
         , HSet s'
         , HSet s''
         , NotMember x s'
         , Intersection' s s' s''
         )
           => Intersection' (HCons x s) s' s''
 where
  intersection' (HCons _ s) s' = s `intersection'` s'
-}

instance ( HSet s
         , HSet s'
         , HSet s''
         , Intersection'' s s' s''
         )
           => Intersection' s s' s''
 where
  intersection' s s' = s `intersection''` s'

class (HSet s, HSet s', HSet s'') => Intersection'' s s' s'' | s s' -> s''
 where
  intersection'' :: s -> s' -> s''

instance ( HSet (HCons x s)
         , HList s
         , HSet s'
         , HSet s''
         , NotMember x s'
         , Intersection' s s' s''
         )
           => Intersection'' (HCons x s) s' s''
 where
  intersection'' (HCons _ s) s' = s `intersection'` s'

class HList l => IsHList l
 where
  isHList :: l -> l
  isHList = id

instance HList l => IsHList l

class HSet s => IsHSet s
 where
  isHSet :: s -> s
  isHSet = id

instance HSet s => IsHSet s

class (HList l, HSet s) => HList2HSet l s | l -> s
 where
  hList2HSet :: l -> s

instance HList2HSet HNil HNil
 where
  hList2HSet HNil = HNil

instance ( HList2HSet l s
         , HList s
         , NotMember e s
         )
      => HList2HSet (HCons e l) (HCons e s)
 where
  hList2HSet (HCons e l) = (HCons e (hList2HSet l))
