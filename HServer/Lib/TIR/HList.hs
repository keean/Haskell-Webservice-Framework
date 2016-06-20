{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Lib.TIR.HList where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved.
------------------------------------------------------------------------------

import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic
import qualified Lib.TIR.Peano as Peano

------------------------------------------------------------------------------
-- types and simple constructors

infixr 1 `HCons`
data HNil = HNil
data HCons e l = HCons e l

infixr 1 `hCons`
hCons :: HExtend l e l' => e -> l -> l'
hCons e l = hExtend l e

hEmpty :: HNil
hEmpty = HNil

type HSingleton e = HCons e HNil

hSingleton :: e -> HSingleton e
hSingleton e = HCons e hEmpty

hFromSingleton :: HSingleton e -> e
hFromSingleton (HCons e HNil) = e

------------------------------------------------------------------------------

class HList r
instance HList HNil 
instance HList r => HList (HCons a r)

------------------------------------------------------------------------------
-- listbuilder

class HBuildList l a r | r -> a l where
	hBuildList :: l -> a -> r
instance HReverse (HCons a l) (HCons a' l') => HBuildList l a (HCons a' l') where
	hBuildList l x = hReverse (HCons x l)
instance HBuildList (HCons a l) b r => HBuildList l a (b -> r) where
	hBuildList l x y = hBuildList (HCons x l) y

hlist :: HBuildList HNil e l => e -> l
hlist x = hBuildList HNil x

hexists :: HCons e l -> HCons e l
hexists t@(HCons _ _) = t

test1 = hexists $ hlist "aaa" 'b' True (Just 'c') False ["aaa"]

------------------------------------------------------------------------------

class HList r => ShowHList r where
	showHList' :: r -> ShowS
instance ShowHList HNil where
	showHList' _ = showChar '>'
instance Show a => ShowHList (HCons a HNil) where
	showHList' (HCons a _) = shows a . showChar '>'
instance (Show a,ShowHList (HCons b r)) => ShowHList (HCons a (HCons b r)) where
	showHList' (HCons a r) = shows a . showChar ',' . showHList' r

showHList :: ShowHList r => r -> ShowS
showHList r = showChar '<' . showHList' r

instance Show HNil where
	showsPrec _ r = showHList r
instance (ShowHList r,ShowHList (HCons a r)) => Show (HCons a r) where
	showsPrec _ r = showHList r

------------------------------------------------------------------------------
-- test for empty list

class (HList r,Logic.MBool t) => HNull r t | r -> t where
	hNull :: r -> t
instance HNull HNil Logic.AllTrue where
	hNull _ = Logic.AllTrue
instance HList r => HNull (HCons a r) Logic.AllFalse where
	hNull _ = Logic.AllFalse

class (HList r,Peano.NotNegative n) => HSize r n | r -> n where
	hSize :: r -> n
instance HSize HNil Peano.Zero where
	hSize _ = Peano.zero
instance HSize r n => HSize (HCons a r) (Peano.Suc n) where
	hSize (_ :: HCons a r) = Peano.Suc $ hSize (undefined :: r)

------------------------------------------------------------------------------
-- List functions

data HListEmpty = HListEmpty

class HList r => HHead r a | r -> a where
	hHead :: r -> a
instance Ctrl.AssertFail HListEmpty => HHead HNil () where
	hHead _ = ()
instance HList r => HHead (HCons a r) a where
	hHead (HCons a _) = a

class (HList r,HList r') => HTail r r' | r -> r' where
	hTail :: r -> r'
instance Ctrl.AssertFail HListEmpty => HTail HNil HNil where
	hTail _ = hEmpty
instance HList r => HTail (HCons a r) r where
	hTail (HCons _ r) = r

class HList r => HLast r a | r -> a where
	hLast :: r -> a
instance Ctrl.AssertFail HListEmpty => HLast HNil () where
	hLast _ = ()
instance HLast (HCons a HNil) a where
	hLast (HCons a _) = a
instance HLast (HCons c r) a => HLast (HCons b (HCons c r)) a where
	hLast (HCons _ r) = hLast r

class (HList r,HList r') => HInit r r' | r -> r' where
	hInit :: r -> r'
instance Ctrl.AssertFail HListEmpty => HInit HNil HNil where
	hInit _ = hEmpty
instance HInit (HCons a HNil) HNil where
	hInit (HCons _ _) = hEmpty
instance HInit (HCons b r) r' => HInit (HCons a (HCons b r)) (HCons a r') where
	hInit (HCons a r) = HCons a (hInit r)

class (HList r,HList r') => HSnoc r a r' | r a -> r', r r' -> a where
	hSnoc :: r -> a -> r'
instance HSnoc HNil a (HCons a HNil) where
	hSnoc _ a = HCons a hEmpty
instance HSnoc  r a r' => HSnoc (HCons a' r) a (HCons a' r') where
	hSnoc (HCons a r) a' = HCons a (hSnoc r a')

class HList r => HExtend r a r' | r a -> r' where
	hExtend :: r -> a -> r'
instance HExtend HNil a (HCons a HNil) where
	hExtend _ a = HCons a hEmpty
instance HList r => HExtend (HCons b r) a (HCons a (HCons b r)) where
	hExtend r a = HCons a r

class HList l => HReverse l l' | l -> l',l' -> l where
	hReverse :: l -> l'
instance (HReverseAcc l HNil l',HReverseAcc l' HNil l) => HReverse l l' where
	hReverse l = hReverseAcc l HNil

class (HList l,HList l') => HReverseAcc l l' l'' | l l' -> l'' where
	hReverseAcc :: l -> l' -> l''
instance HList l => HReverseAcc HNil l l where
	hReverseAcc _ l = l
instance (HList l',HReverseAcc l (HCons a l') l'') => HReverseAcc (HCons a l) l' l'' where
	hReverseAcc (HCons a l) l' = hReverseAcc l (HCons a l')

------------------------------------------------------------------------------

-- Apply a polymorphic hMap for all types
class (HList r,HList r') => HMap f r r' | f r -> r' where
	hMap :: f -> r -> r'
instance HMap f HNil HNil where
	hMap _ _ = hEmpty
instance (Ctrl.Apply f a b,HMap f r r') => HMap f (HCons a r) (HCons b r') where
	hMap f (HCons a r) = HCons (Ctrl.apply f a) (hMap f r)

class HList l => HMarkAll c l m | c l -> m where
   hMarkAll :: (forall a . a -> c a) -> l -> m
instance HMarkAll c HNil HNil where
   hMarkAll _ _ = HNil
instance HMarkAll c l m => HMarkAll c (HCons e l) (HCons (c e) m) where
   hMarkAll c (HCons e l) = HCons (c e) (hMarkAll c l)


------------------------------------------------------------------------------

-- A polymorphic hFold for all types
class HList r => HFold f v r w | f v r -> w where
	hFold :: f -> v -> r -> w
instance HFold  f v HNil v where
	hFold _ v _ = v
instance (HFold f v r w,Ctrl.Apply f (a,w) w') => HFold f v (HCons a r) w' where
	hFold f v (HCons a r) = Ctrl.apply f (a,hFold f v r)

------------------------------------------------------------------------------

class (HList r1,HList r2,HList r3) => HZip r1 r2 r3 | r1 r2 -> r3, r1 r3 -> r2, r2 r3 -> r1 where
	hZip :: r1 -> r2 -> r3
instance HZip HNil HNil HNil where
	hZip _ _ = hEmpty
instance HZip r1 r2 r3 => HZip (HCons a r1) (HCons b r2) (HCons (a,b) r3) where
	hZip (HCons a r) (HCons b s) = HCons (a,b) (hZip r s)

class (HList r1,HList r2,HList r3) => HUnzip r1 r2 r3 | r1 -> r2 r3, r1 r3 -> r2, r2 r3 -> r1 where
	hUnzip :: r1 -> (r2,r3)
instance HUnzip HNil HNil HNil where
	hUnzip _ = (hEmpty,hEmpty)
instance HUnzip r1 r2 r3 => HUnzip (HCons (a,b) r1) (HCons a r2) (HCons b r3) where
	hUnzip (HCons (a,b) r1) = (HCons a r2,HCons b r3) where
		(r2,r3) = hUnzip r1

------------------------------------------------------------------------------
-- implements order preserving multi-set union.

class (HList r1,HList r2,HList r3) => HAppend r1 r2 r3 | r1 r2 -> r3, r1 r3 -> r2 where
	hAppend :: r1 -> r2 -> r3
instance HAppend HNil HNil HNil where
	hAppend _ _ = hEmpty
instance HList r => HAppend HNil (HCons a r) (HCons a r) where
	hAppend _ r = r
instance HAppend r1 r2 r3 => HAppend (HCons a r1) r2 (HCons a r3) where
	hAppend (HCons a r1) r2 = HCons a (r1 `hAppend`  r2)

------------------------------------------------------------------------------

class HList r => Tuple t r | t -> r , r -> t where
	hFromTuple :: t -> r
	hToTuple :: r -> t

instance Tuple (a,b) (HCons a (HCons b HNil)) where
	hFromTuple (a,b) = HCons a (HCons b hEmpty)
	hToTuple (HCons a (HCons b _)) = (a,b)

instance Tuple (a,b,c) (HCons a (HCons b (HCons c HNil))) where
	hFromTuple (a,b,c) = HCons a (HCons b (HCons c hEmpty))
	hToTuple (HCons a (HCons b (HCons c _))) = (a,b,c)

instance Tuple (a,b,c,d) (HCons a (HCons b (HCons c (HCons d HNil)))) where
	hFromTuple (a,b,c,d) = HCons a (HCons b (HCons c (HCons d hEmpty)))
	hToTuple (HCons a (HCons b (HCons c (HCons d _)))) = (a,b,c,d)

instance Tuple (a,b,c,d,e) (HCons a (HCons b (HCons c (HCons d (HCons e HNil))))) where
	hFromTuple (a,b,c,d,e) = HCons a (HCons b (HCons c (HCons d (HCons e hEmpty))))
	hToTuple (HCons a (HCons b (HCons c (HCons d (HCons e _))))) = (a,b,c,d,e)

instance Tuple (a,b,c,d,e,f) (HCons a (HCons b (HCons c (HCons d (HCons e (HCons f HNil)))))) where
	hFromTuple (a,b,c,d,e,f) = HCons a (HCons b (HCons c (HCons d (HCons e (HCons f hEmpty))))) 
	hToTuple (HCons a (HCons b (HCons c (HCons d (HCons e (HCons f _)))))) = (a,b,c,d,e,f)

