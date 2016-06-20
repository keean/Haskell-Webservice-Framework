{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Lib.TIR.Experiment where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved.
------------------------------------------------------------------------------

import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic
import qualified Lib.TIR.Peano as Peano
import Lib.TIR.HList as HList
import Lib.TIR.HArray as HArray
import Lib.TIR.HType as HType
-- import Lib.TIR.TypeEq as TypeEq

------------------------------------------------------------------------------
{-
class HContainsType r a t | r a -> t where
	hContainsType :: r -> a -> t
instance HContainsType HNil a Logic.AllFalse where
	hContainsType _ _ = Logic.AllFalse
instance (Logic.TypeEq a b l,HContainsType' l r a t) => HContainsType (HCons b r) a t where
	hContainsType (_::HCons b r) a = hContainsType' (Logic.typeEq a (undefined::b)) (undefined::r) a
class HContainsType' l r a t | l r a -> t where
	hContainsType' :: l -> r -> a -> t
instance HContainsType' Logic.AllTrue r a Logic.AllTrue where
	hContainsType' _ _ _ = Logic.AllTrue
instance HContainsType r a t => HContainsType' Logic.AllFalse r a t where
	hContainsType' _ r a = hContainsType r a

class (HList r,HList s,Logic.Modal t) => HSubType r s t | r s -> t where
	hSubType :: r -> s -> t
instance HList r => HSubType r HNil Logic.AllTrue where
	hSubType _ _ = Logic.AllTrue
instance (HSubType r s t2,HContainsType r a t3,Logic.And t2 t3 t) => HSubType r (HCons a s) t where
	hSubType r (_::HCons a s) = hSubType r (undefined::s) `Logic.and` hContainsType r (undefined::a)
-}
------------------------------------------------------------------------------
{-
class (HList r,Peano.NotNegative n) => HCount r a n | r a -> n where
	hCount :: r -> a -> n
instance HCount HNil a Peano.Zero where
	hCount _ _ = Peano.zero
instance HCount r a n => HCount (HCons a r) a (Peano.Suc n) where
	hCount (_ :: HCons b r) a = Peano.Suc (hCount (undefined :: r) a)
instance HCount r a n => HCount (HCons b r) a n where
	hCount (_ :: HCons b r) a = hCount (undefined :: r) a
-}

-- Array functions

{- uses TypeEq -}
{-
class (HList l,Peano.NotNegative n) => HIndexOf l a n | l a -> n where
	hIndexOf :: l -> a -> n
instance Ctrl.AssertFail (TypeNotFound a) => HIndexOf HNil a Peano.NaN where
	hIndexOf _ _ = Peano.NaN
instance (Logic.TypeEq a b t,HIndexOf' t l a n) => HIndexOf (HCons b l) a n where
	hIndexOf (_::HCons b l) a = hIndexOf' (Logic.typeEq a (undefined::b)) (undefined::l) a 
class (HList l,Peano.NotNegative n) => HIndexOf' t l a n | t l a -> n where
	hIndexOf' :: t -> l -> a -> n
instance HList l => HIndexOf' Logic.AllTrue l a Peano.Zero where
	hIndexOf' _ _ _ = Peano.zero
instance HIndexOf l a n => HIndexOf' Logic.AllFalse l a (Peano.Suc n) where
	hIndexOf' _ l a = Peano.Suc (hIndexOf l a)
-}

------------------------------------------------------------------------------


{-
class (HList r,HList r') => HSelectAll r a r' | r a -> r' where
   hSelectAll :: r -> a -> r'
instance HSelectAll HNil a HNil where
	hSelectAll _ _ = hEmpty
instance HSelectAll r a r' => HSelectAll (HCons a r) a (HCons a r') where
   hSelectAll (HCons b r) a = HCons b (hSelectAll r a)
instance HSelectAll r a r' => HSelectAll (HCons b r) a r' where
   hSelectAll (HCons _ r) a = hSelectAll r a
-}

{-
class (HList l,HList m,HList l') => HSelectAllByHList l m l' | l m -> l' where
	hSelectAllByHList :: l -> m -> l'
instance HList m => HSelectAllByHList HNil m HNil where
	hSelectAllByHList _ _ = HNil
instance (HContainsType m a t,HSelectAllByHList' t a l m l') => HSelectAllByHList (HCons a l) m l' where
	hSelectAllByHList (HCons a l) m = hSelectAllByHList' (hContainsType m a) a l m
class (HList l,HList m,HList l',Logic.Modal t) => HSelectAllByHList' t a l m l' | t a l m -> l' where
	hSelectAllByHList' :: t -> a -> l -> m -> l'
instance HSelectAllByHList l m l' => HSelectAllByHList' Logic.AllTrue a l m (HCons a l') where
	hSelectAllByHList' _ a l m = HCons a (hSelectAllByHList l m)
instance HSelectAllByHList l m l' => HSelectAllByHList' Logic.AllFalse a l m l' where
	hSelectAllByHList' _ _ l m = hSelectAllByHList l m
-}

class HDeleteFst' l e HNil l' => HDeleteFst l e l' where
	hDeleteFst :: l -> e -> l'
instance HDeleteFst' l e HNil l' => HDeleteFst l e l' where
	hDeleteFst l e = hDeleteFst' l e HNil
class HList l => HDeleteFst' l e acc l' where
	hDeleteFst' :: l -> e -> acc -> l'
instance (HList (HCons e l),HReverseAcc acc l l') => HDeleteFst' (HCons e l) e acc l' where
	hDeleteFst' (HCons _ l) _ acc = hReverseAcc acc l
instance (HDeleteFst' l e (HCons e' acc) l') => HDeleteFst' (HCons e' l) e acc l' where
	hDeleteFst' (HCons e' l) e acc = hDeleteFst' l e (HCons e' acc)


{- uses TypeEq -}
{-
class (HList l,HList l') => HDeleteAll l a l' | l a -> l' where
	hDeleteAll :: l -> a -> l'
instance HDeleteAll HNil a HNil where
	hDeleteAll _ _ = HNil
instance (Logic.TypeEq a b t,HDeleteAll' t b l a l') => HDeleteAll (HCons b l) a l' where
	hDeleteAll (HCons b l) a = hDeleteAll' (Logic.typeEq a b) b l a
class (HList l,HList l',Logic.MBool t) => HDeleteAll' t b l a l' | t b l a -> l' where
	hDeleteAll' :: t -> b -> l -> a -> l'
instance HDeleteAll l a l' => HDeleteAll' Logic.AllTrue a l a l' where
	hDeleteAll' _ _ l a = hDeleteAll l a
instance HDeleteAll l a l' => HDeleteAll' Logic.AllFalse b l a (HCons b l') where
	hDeleteAll' _ b l a = HCons b (hDeleteAll l a)
-}

{-
class (HList l,HList m,HList l') => HDeleteAllByHList l m l' | l m -> l' where
	hDeleteAllByHList :: l -> m -> l'
instance HList m => HDeleteAllByHList HNil m HNil where
	hDeleteAllByHList _ _ = HNil
instance (HMember m a t,HDeleteAllByHList' t a l m l') => HDeleteAllByHList (HCons a l) m l' where
	hDeleteAllByHList (HCons a l) m = hDeleteAllByHList' (hMember m a) a l m
class (HList l,HList m,HList l',Logic.Modal t) => HDeleteAllByHList' t a l m l' | t a l m -> l' where
	hDeleteAllByHList' :: t -> a -> l -> m -> l'
instance HDeleteAllByHList l m l' => HDeleteAllByHList' Logic.SomeTrue a l m l' where
	hDeleteAllByHList' _ _ l m = hDeleteAllByHList l m
instance HDeleteAllByHList l m l' => HDeleteAllByHList' Logic.AllFalse a l m (HCons a l') where
	hDeleteAllByHList' _ a l m = HCons a (hDeleteAllByHList l m)
-}

------------------------------------------------------------------------------

{-
class (HList l,HList l') => HAfterFst l a l' | l a -> l' where
	hAfterFst :: l -> a -> l'
instance HAfterFst HNil a HNil where
	hAfterFst _ _ = HNil
instance HList l => HAfterFst (HCons a l) a l where
	hAfterFst (HCons _ l) _ = l
instance HAfterFst l a l' => HAfterFst (HCons b l) a l' where
	hAfterFst (HCons _ l) a = hAfterFst l a
-}

{- uses TypeEq -}
{-
class (HList l,HList l') => HBeforeFst l a l' | l a -> l' where
	hBeforeFst :: l -> a -> l'
instance HBeforeFst HNil a HNil where
	hBeforeFst _ _ = HNil
instance (Logic.TypeEq a b t,HBeforeFst' t b l a l') => HBeforeFst (HCons b l) a l' where
	hBeforeFst (HCons b l) a = hBeforeFst' (Logic.typeEq a b) b l a
class (HList l,HList l',Logic.MBool t) => HBeforeFst' t b l a l' | t b l a -> l' where
	hBeforeFst' :: t -> b -> l -> a -> l'
instance HList l => HBeforeFst' Logic.AllTrue a l a HNil where
	hBeforeFst' _ _ _ _ = HNil
instance HBeforeFst l a l' => HBeforeFst' Logic.AllFalse b l a (HCons b l') where
	hBeforeFst' _ b l a = HCons b (hBeforeFst l a)
-}

------------------------------------------------------------------------------

{-
class HList r => HInject r a where
	hInject :: r -> a -> r
instance Ctrl.AssertFail TypeNotPresent => HInject HNil a where
	hInject _ _ = hEmpty
instance HList r => HInject (HCons a r) a where
	hInject (HCons _ r) a = HCons a r
instance HInject r a => HInject (HCons b r) a where
	hInject (HCons b r) a = HCons b (hInject r a)
-}

------------------------------------------------------------------------------
{-
class (HList r,HList r') => HInsert r a r' | r a -> r', r r' -> a where
	hInsert :: r -> a -> r'
instance HInsert HNil a (HCons a HNil) where
	hInsert r a = HCons a r
instance HList r => HInsert (HCons a r) a (HCons a r) where
	hInsert (HCons _ r) a = HCons a r
instance HInsert r a r' => HInsert (HCons b r) a (HCons b r') where
	hInsert (HCons b r) a = HCons b (hInsert r a)
-}

{- uses TypeEq -}
{-
class (HList l,HList l') => HReplaceFst l a b l' | l a b -> l' where
	hReplaceFst :: l -> a -> b -> l'
instance Ctrl.AssertFail TypeNotPresent => HReplaceFst HNil a b HNil where
	hReplaceFst _ _ _ = HNil
instance (Logic.TypeEq a c t,HReplaceFst' t c l a b l') => HReplaceFst (HCons c l) a b l' where
	hReplaceFst (HCons c l) a b = hReplaceFst' (Logic.typeEq a c) c l a b
class (HList l,HList l',Logic.MBool t) => HReplaceFst' t c l a b l' | t c l a b -> l' where
	hReplaceFst' :: t -> c -> l -> a -> b -> l'
instance HList l => HReplaceFst' Logic.AllTrue a l a b (HCons b l) where
	hReplaceFst' _ _ l _ b = HCons b l
instance HReplaceFst l a b l' => HReplaceFst' Logic.AllFalse c l a b (HCons c l') where
	hReplaceFst' _ c l a b = HCons c (hReplaceFst l a b)
-}

------------------------------------------------------------------------------

{-
class (HList r,HList r') => HUnique r r' | r -> r' where
	hUnique :: r -> r'
instance HUnique HNil HNil where
	hUnique _ = hEmpty
instance (HDeleteAll r a r2,HUnique r2 r') => HUnique (HCons a r) (HCons a r') where
	hUnique (HCons a r) = HCons a (hUnique (hDeleteAll r a))
-}

------------------------------------------------------------------------------

{- Requires Remove -}
{-
class (HList r1,HList r2,HList r3) => HExcept r1 r2 r3 | r1 r2 -> r3 where
	hExcept :: r1 -> r2 -> r3
instance HExcept HNil HNil HNil where
	hExcept _ _ = hEmpty
instance HList r => HExcept r HNil r where
	hExcept r _ = r
instance (HDeleteAll r1 a r1Ea,
		HDeleteAll r2 a r2Ea,
		HExcept r1Ea r2Ea r1Dr2) => HExcept r1 (HCons a r2) r1Dr2 where
	hExcept r1 (HCons a r2) = hDeleteAll r1 a `hExcept` hDeleteAll r2 a
-}

{- Requires Remove -}
{-
class (HList r1,HList r2,HList r3) => HNotCommon r1 r2 r3 | r1 r2 -> r3 where
	hNotCommon :: r1 -> r2 -> r3
instance HNotCommon HNil HNil HNil where
	hNotCommon _ _ = hEmpty
instance HList r => HNotCommon HNil (HCons a r) (HCons a r) where
	hNotCommon _ r = r
instance HList r => HNotCommon (HCons a r) HNil (HCons a r) where
	hNotCommon r _ = r
instance (HSelectAll r1 a r1Sa,
		HSelectAll r2 a r2Sa,
		HNull r1Sa t,
		Logic.Conditional t (HCons a r2Sa) HNil r1Cr2,
		HDeleteAll r1 a r1Ea,
		HDeleteAll r2 a r2Ea,
		HNotCommon r1Ea r2Ea r1Dr2,
		HConcat r1Cr2 r1Dr2 r3) => HNotCommon r1 (HCons a r2) r3 where
	hNotCommon r1 (HCons a r2) = (\r1Sa r2Sa -> Logic.cond (hNull r1Sa) (HCons a r2Sa) hEmpty
		`hConcat` (hDeleteAll r1 a `hNotCommon` hDeleteAll r2 a)) (hSelectAll r1 a) (hSelectAll r2 a)
-}
	
{- Requires Remove -}
{-
class (HList r1,HList r2,HList r3) => HLeftCommon r1 r2 r3 | r1 r2 -> r3 where
	hLeftCommon :: r1 -> r2 -> r3
instance HLeftCommon HNil HNil HNil where
	hLeftCommon _ _ = hEmpty
instance HList r => HLeftCommon HNil (HCons a r) HNil where
	hLeftCommon _ _ = hEmpty
instance HList r => HLeftCommon (HCons a r) HNil HNil where
	hLeftCommon _ _ = hEmpty
instance (HSelectAll r1 a r1Sa,
		HDeleteAll r1 a r1Ea,
		HDeleteAll r2 a r2Ea,
		HLeftCommon r1Ea r2Ea r1Sr2',
		HConcat r1Sa r1Sr2' r1Sr2) => HLeftCommon r1 (HCons a r2) r1Sr2 where
	hLeftCommon r1 (HCons a r2) = hSelectAll r1 a `hConcat` (hDeleteAll r1 a `hLeftCommon` hDeleteAll r2 a)
-}

{- Requires Remove -}
{-
class (HList r1,HList r2,HList r3) => HCommon r1 r2 r3 | r1 r2 -> r3 where
	hCommon :: r1 -> r2 -> r3
instance HCommon HNil HNil HNil where
	hCommon _ _ = hEmpty
instance HList r => HCommon HNil (HCons a r) HNil where
	hCommon _ _ = hEmpty
instance HList r => HCommon (HCons a r) HNil HNil where
	hCommon _ _ = hEmpty
instance (HSelectAll r1 a r1Sa,
		HSelectAll r2 a r2Sa,
		HNull r1Sa t,
		HConcat r1Sa (HCons a r2Sa) r1Sr2,
		Logic.Conditional t HNil r1Sr2 r1Cr2,
		HDeleteAll r1 a r1Ea,
		HDeleteAll r2 a r2Ea,
		HCommon r1Ea r2Ea r1Ir2',
		HConcat r1Cr2 r1Ir2' r3) => HCommon r1 (HCons a r2) r3 where
	hCommon r1 (HCons a r2) = (\r1Sa -> Logic.cond (hNull r1Sa) hEmpty (r1Sa `hConcat` HCons a (hSelectAll r2 a))
		`hConcat` (hDeleteAll r1 a `hCommon` hDeleteAll r2 a)) (hSelectAll r1 a)
-}

------------------------------------------------------------------------------
	
{-
class (HOccurs x l,HDel l x i m,HOccurs y m)
		=> TTuple l m i x y where
	tuple :: l -> ((i,m),(x,y))
instance (HOccurs x l,HDel l x i m,HOccurs y m)
		=> TTuple l m i x y where
	tuple l = let
		x = hOccurs l
		(i,m) = hDel l x
		y = hOccurs m
		in ((i,m),(x,y))
-}

{-
tuple' :: (HOccurs x l,HDeleteFst l x m,HOccurs y m) => l -> (m,(x,y))
tuple' l = let
	x = hOccurs l
	m = hDeleteFst l x
  	y = hOccurs m
  in (m,(x,y))
  -}

tuple l = let
	x = hOccurs l
	m = hDeleteFst l x
	y = hOccurs m
  in (m,(x,y))

{-
tuple :: (HOccurs x l,HDel l x i m,HOccurs y m) => l -> ((i,m),(x,y))
tuple l = let
	x = hOccurs l
	(i,m) = hDel l x
  	y = hOccurs m
  in ((i,m),(x,y))
-}

tst :: HOccurs e l => l -> e
tst x = hOccurs x

one :: Int
one = 1

inc :: Int -> Int
inc i = i+1

oneTrue :: HCons Int (HCons Bool HNil)
oneTrue = HCons one (HCons True HNil)

------------------------------------------------------------------------------

data NS1 = NS1 deriving Show
data NS2 = NS2 deriving Show

class CheckLabel l n m | l -> n m where
	checkLabel :: l -> Label n m 
instance CheckLabel (HCons (Label n a) HNil) n (HCons a HNil) where
	checkLabel (HCons (Label n a) _) = Label n (HCons a HNil)
instance CheckLabel (HCons x l) n l' => CheckLabel (HCons (Label n a) (HCons x l)) n (HCons a l') where
	checkLabel (HCons (Label n a) l) = Label n (HCons a l') where
		Label n l' = checkLabel l

--

class NewTypeEq x y t where
	newTypeEq :: x -> y -> t
instance Ctrl.Force Logic.AllTrue t => NewTypeEq x x t where
	newTypeEq _ _ = Ctrl.force Logic.AllTrue
instance Ctrl.Force Logic.AllFalse t => NewTypeEq x y t where
	newTypeEq _ _ = Ctrl.force Logic.AllFalse

------------------------------------------------------------------------------

class TTypeable x n | x -> n where
	ttypeable :: x -> n
instance TTypeable () Peano.Zero where
	ttypeable _ = Peano.zero
instance TTypeable Bool Peano.One where
	ttypeable _ = Peano.one
instance TTypeable Char Peano.Two where
	ttypeable _ = Peano.two
instance TTypeable Int Peano.Three where
	ttypeable _ = Peano.three
instance TTypeable Integer Peano.Four where
	ttypeable _ = Peano.four
instance TTypeable Float Peano.Five where
	ttypeable _ = Peano.five
instance TTypeable Double Peano.Six where
	ttypeable _ = Peano.six

class HList l => HTypeToNat l e i | l e -> i where
	hTypeToNat :: l -> e -> i
instance HTypeToNat HNil e Peano.NaN where
	hTypeToNat _ _ = Peano.NaN
instance (TTypeable e n,TTypeable e' n',Peano.Eq n n' t,HTypeToNat' t l e i)
		=> HTypeToNat (HCons e' l) e i where
	hTypeToNat (HCons e' l) e = hTypeToNat' (Peano.eq (ttypeable e) (ttypeable e')) l e 

class (Logic.MBool t,HList l) => HTypeToNat' t l e i | t l e -> i where
	hTypeToNat' :: t -> l -> e -> i
instance HList l => HTypeToNat' Logic.AllTrue l e Peano.Zero where
	hTypeToNat' _ _ _ = Peano.zero
instance HTypeToNat l e i => HTypeToNat' Logic.AllFalse l e (Peano.Suc i) where
	hTypeToNat' _ l e = Peano.Suc $ hTypeToNat l e

class (HTypeToNat l e i,HDelete l i l') => HDeleteByType' l e i l' | l e i -> l' where
	hDeleteByType' :: l -> e -> (i,l')
instance (HTypeToNat l e i,HDelete l i l') => HDeleteByType' l e i l' where
	hDeleteByType' l e = let i = hTypeToNat l e in (i,hDelete l i)

hDeleteByType :: HDeleteByType' l e i l' => l -> e -> l'
hDeleteByType l e = snd $ hDeleteByType' l e 
