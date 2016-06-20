{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module TPrelude where

import TTypeable
import TTypeLift

-- Definitions for prelude types
$(ttypeable ''Bool)
$(ttypeable ''Int)
$(ttypeable ''Integer)
$(ttypeable ''Char)
$(ttypeable ''Float)
$(ttypeable ''Double)
$(ttypeable ''[])
$(ttypeable ''Maybe)
$(ttypeable ''Either)
$(ttypeable ''IO)
$(ttypeable ''(->))
$(ttypeable ''())
$(ttypeable ''(,))

-- Definitions for TTypeable
$(ttypeable ''TTrue)
$(ttypeable ''TFalse)
$(ttypeable ''NNil)
$(ttypeable ''N0)
$(ttypeable ''N1)
$(ttypeable ''N2)
$(ttypeable ''N3)
$(ttypeable ''N4)
$(ttypeable ''N5)
$(ttypeable ''N6)
$(ttypeable ''N7)
$(ttypeable ''N8)
$(ttypeable ''N9)
$(ttypeable ''Label)
$(ttypeable ''TRep)
$(ttypeable ''TNil)
$(ttypeable ''TCons)
$(ttypeable ''Nil)
$(ttypeable ''Cons)
$(ttypeable ''(:/\:))
$(ttypeable ''(:\/:))

-- Example higher kinded definitions
data Fix f = Fix (f (Fix f)) deriving Show
$(ttypeable ''Fix)

-- function lifting
$(ttypelift [d|

	data TEither a b = TLeft a | TRight b
	-- type T1 a b = TEither a b
	-- type T2 a b = [TEither a b] |])

	-- type MyList a = [a]	
	data Castable = forall x . Castable' x 
	type HList = [Castable]
	data Castable2 = forall x . Castable2' x
	type HTest a = TEither a [Castable2]

	hHead :: HList -> Castable
	hHead (a:_) = a

	hTail :: HList -> HList
	hTail (_:l) = l

	hLast :: HList -> Castable
	hLast [a] = a
	hLast (_:(a:l)) = hLast (a:l) |])

	-- hOccursMany :: HList -> [QMaybe Castable]
	-- hOccursMany [] = []
	-- hOccursMany (h:t) = QJust h : hOccursMany t |])


{-
	head (HCons a _) = a

	tail (HCons _ l) = l

	last (HCons a HNil) = a
	last (HCons _ (HCons b l)) = last (HCons b l)

	init (HCons _ HNil) = HNil
	init (HCons a (HCons b l)) = HCons a (init (HCons b l))

	hExt :: HList -> (forall a . a) -> HList
	hExt l a = HCons a l

	hSnoc :: HList -> (forall a . a) -> HList
	hSnoc HNil a = HCons a HNil
	hSnoc (HCons b l) a = HCons b (hSnoc l a)

	hReverseAcc HNil l = l
	hReverseAcc (HCons a l) l' = hReverseAcc l (HCons a l')

	hReverse l = hReverseAcc l HNil

	hMap :: (forall a . a -> a) -> HList -> HList
	hMap _ HNil = HNil
	hMap f (HCons a l) = HCons (f a) (hMap f l) |])
-}

$(tDecShow [d| data Interface1 a b = Interface1 {
	method1 :: IO a,
	method2 :: IO b }|])

$(tDecShow [d|
	a = 0
	b = 'c'
	method1 = return (a :: Int) |] )

data Record = Record {
	field1 :: Int,
	field2 :: forall a . a -> a }
