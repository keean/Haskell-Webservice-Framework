{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Lib.TIR.HType where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved.
------------------------------------------------------------------------------

import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic
import qualified Lib.TIR.Peano as Peano
import Lib.TIR.HList as HList
import Lib.TIR.HArray as HArray

data HProxy x = HProxy

------------------------------------------------------------------------------
-- type constraints

data TypeNotBound t = TypeNotBound t
class HBoundType l e where
	hBoundType :: l -> e -> l
instance Ctrl.AssertFail (TypeNotBound e) => HBoundType HNil e where
	hBoundType _ e = HNil
instance HList l => HBoundType (HCons e l) e where
	hBoundType l _ = l
instance HBoundType l e => HBoundType (HCons e' l) e where
	hBoundType (HCons e' l) e = HCons e' (hBoundType l e)

data TypeNotFree t = TypeNotFree t
class HFreeType l e where
	hFreeType :: l -> e -> l
instance HFreeType HNil e where
	hFreeType _ _ = HNil
instance Ctrl.AssertFail (TypeNotFree e) => HFreeType (HCons e l) e where
	hFreeType l e = l
instance HFreeType l e => HFreeType (HCons e' l) e where
	hFreeType (HCons e' l) e = HCons e' (hFreeType l e)

-- Length bounded arrays
data BoundedTooLong t = BoundedTooLong t
class HBounded l n where
	hBounded :: l -> n -> l
instance (HSize l m,Peano.Lt m n t,Logic.AssertTrue t (BoundedTooLong l)) => HBounded l n where
	hBounded l _ = l

-- Sets
class HList l => HSet l where
	hSet :: l -> l
instance HSet HNil where
	hSet = id
instance (HSet l,HFreeType l e) => HSet (HCons e l) where
	hSet (HCons e l) = HCons e (hSet l)

------------------------------------------------------------------------------
 
newtype TypeNotPresent a = TypeNotPresent a

class HList l => HIndexWith l a t w where
	hIndexWith :: l -> a -> t -> w
instance HIndexWith' l a Peano.Zero t w => HIndexWith l a t w where
	hIndexWith l a t = hIndexWith' l a Peano.zero t
class (HList l,Peano.NotNegative n) => HIndexWith' l a n t w where
	hIndexWith' :: l -> a -> n -> t -> w
instance (Ctrl.AssertFail (TypeNotPresent a),Peano.NotNegative n) => HIndexWith' HNil a n t w where
	hIndexWith' _ _ _ _ = undefined
instance (Ctrl.Cont t n w,HList l,Peano.NotNegative n) => HIndexWith' (HCons a l) a n t w where
	hIndexWith' _ _ n k = Ctrl.cont k n
instance (Peano.NotNegative n,HIndexWith' l a (Peano.Suc n) t w) => HIndexWith' (HCons b l) a n t w where
	hIndexWith' (HCons _ l) a n t = hIndexWith' l a (Peano.Suc n) t

class HList l => HIndexIntegral l a where
	hIndexIntegral :: Integral i => l -> a -> i
instance HList l => HIndexIntegral (HCons a l) a where
	hIndexIntegral _ _ = 0
instance HIndexIntegral l a => HIndexIntegral (HCons b l) a where
	hIndexIntegral (HCons _ l) a = 1 + (hIndexIntegral l a)

class HList l => HIndex l a i w where
	hIndex :: l -> a -> i -> w
instance (HList (HCons a l),Peano.NotNegative i,Ctrl.Force i j) => HIndex (HCons a l) a i j where
	hIndex _ _ i = Ctrl.force i
instance (HIndex l a (Peano.Suc i) j) => HIndex (HCons a' l) a i j where
	hIndex (HCons _ l) a i = hIndex l a (Peano.Suc i)

{-
class (HIndex l e Peano.Zero n,HDelete l n l') => HDel l e n l' where
	hDel :: l -> e -> (n,l') 
instance (HIndex l e Peano.Zero n,HDelete l n l') => HDel l e n l' where
	hDel l e = let n = hIndex l e Peano.zero in (n,hDelete l n)
-}

{-
class HDeleteByType' l l a Peano.Zero l' => HDeleteByType l a l' where
	hDeleteByType :: l -> a -> l'
instance HDeleteByType' l l a Peano.Zero l' => HDeleteByType l a l' where
	hDeleteByType l a = hDeleteByType' l l a Peano.Zero
class HDeleteByType' k l a i w where
	hDeleteByType' :: k -> l -> a -> i -> w
instance (HList (HCons a l),Peano.NotNegative i,HDelete k i j) => HDeleteByType' k (HCons a l) a i j where
	hDeleteByType' k _ _ i = hDelete k i
instance (HDeleteByType' k l a (Peano.Suc i) j) => HDeleteByType' k (HCons a' l) a i j where
	hDeleteByType' k (HCons a' l) a i = hDeleteByType' k l a (Peano.Suc i)
-}

------------------------------------------------------------------------------

class HList l => HOccurs e l where
	hOccurs :: l -> e
instance Ctrl.Force e e' => HOccurs e' (HCons e HNil) where
	hOccurs (HCons e _) = Ctrl.force e
instance HOccurs' e (HCons e' (HCons x l)) => HOccurs e (HCons e' (HCons x l)) where
	hOccurs l = hOccurs' l
class HList l => HOccurs' e l where
   hOccurs' :: l -> e
instance (HList l,HFreeType l e) => HOccurs' e (HCons e l) where
   hOccurs' (HCons e _) = e
instance HOccurs' e l => HOccurs' e (HCons e' l) where
   hOccurs' (HCons _ l) = hOccurs' l

class HList r => HOccursFst r a where
	hOccursFst :: r -> a
instance Ctrl.AssertFail (TypeNotPresent a) => HOccursFst HNil a where
	hOccursFst _ = undefined
instance HList r => HOccursFst (HCons a r) a where
	hOccursFst (HCons a _) = a
instance HOccursFst r a => HOccursFst (HCons a' r) a where
	hOccursFst (HCons _ r) = hOccursFst r

class HList r => HOccursMany r a where
	hOccursMany :: r -> [a]
instance HOccursMany HNil a where
	hOccursMany _ = []
instance HOccursMany r a => HOccursMany (HCons a r) a where
	hOccursMany (HCons a r) = a:hOccursMany r
instance HOccursMany r b => HOccursMany (HCons a r) b where
	hOccursMany (HCons _ r) = hOccursMany r

class HList r => HOccursMany1 r a where
	hOccursMany1 :: r -> (a,[a])
instance Ctrl.AssertFail (TypeNotPresent a) => HOccursMany1 HNil a where
	hOccursMany1 _ = (undefined,[])
instance HOccursMany r a => HOccursMany1 (HCons a r) a where
	hOccursMany1 (HCons a r) = (a,hOccursMany r)
instance HOccursMany1 r a => HOccursMany1 (HCons a' r) a where
	hOccursMany1 (HCons _ r) = hOccursMany1 r

------------------------------------------------------------------------------

-- Apply hFold function to matching type only
class HList r => HFold1 r a where
	hFold1 :: (a -> b -> b) -> b -> r -> b
instance HFold1 HNil a where
	hFold1 _ v _ = v
instance HFold1 r a => HFold1 (HCons b r) a where
	hFold1 f v (HCons _ r) = hFold1 f v r
instance HFold1 r a => HFold1 (HCons a r) a where
	hFold1 f v (HCons a r) = f a (hFold1 f v r)

------------------------------------------------------------------------------

class (HList l,HList m) => HSelectFstByHList l m l' | l m -> l' where
	hSelectFstByHList :: l -> m -> l'
instance HList l => HSelectFstByHList l HNil HNil where
	hSelectFstByHList _ _ = HNil
instance (HOccursFst l a,HSelectFstByHList l k l') => HSelectFstByHList l (HCons a k) (HCons a l') where
	hSelectFstByHList l (HCons _ k) = HCons (hOccursFst l) (hSelectFstByHList l k)

------------------------------------------------------------------------------

class HAssertType s r | s -> r where
	hAssertType :: s -> r -> ()
instance HAssertType HNil HNil where
	hAssertType _ _ = ()
instance HAssertType s r => HAssertType (HCons x s) (HCons x r) where
	hAssertType (HCons _ s) (HCons _ r) = hAssertType (Ctrl.typeId s) (Ctrl.typeId r)

