{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- ArrowIO

module Lib.Arrow.StateFunctor(StateFunctor(..)) where

import Control.Arrow
import Lib.Arrow.ArrowState
import Lib.Arrow.ArrowFunctor

--------------------------------------------------------------------
-- State Functor

newtype StateFunctor s a b c = SF { runSF :: a (b,s) (c,s) }

instance (Arrow a,Arrow (StateFunctor s a)) => ArrowFunctor (StateFunctor s) a where
	up f = SF $ first f
	down (SF f) = arr (\a -> (a,undefined)) >>> f >>> arr (\(a,_) -> a)

instance ArrowChoice a => Arrow (StateFunctor s a) where
	arr f = up $ arr f
	SF f >>> SF g = SF $ f >>> g
	first (SF f) = SF $ arr (\((b,d),s) -> ((b,s),d))
		>>> first f
		>>> arr (\((c,s),d) -> ((c,d),s))

instance (Arrow a,ArrowChoice a) => ArrowState s (StateFunctor s a) where
	fetch = SF $ arr (\(_,s) -> (s,s))
	store = SF $ arr (\(x,_) -> ((),x))

instance ArrowChoice a => ArrowChoice (StateFunctor s a) where
	left (SF f) = SF $ arr (\(z,s) -> case z of
			Left b -> Left (b,s)
			Right c -> Right (c,s)) >>>
		((f >>> first (arr Left)) ||| first (arr Right))

instance (ArrowChoice a,ArrowZero a) => ArrowZero (StateFunctor s a) where
	zeroArrow = SF zeroArrow

instance (ArrowChoice a,ArrowPlus a) => ArrowPlus (StateFunctor s a) where
	SF f <+> SF g = SF $ f <+> g

instance (ArrowChoice a,ArrowApply a) => ArrowApply (StateFunctor s a) where
	app = SF $ arr (\((SF f,b),s) -> (f,(b,s))) >>> app

instance (ArrowState s a,ArrowFunctor f a,Arrow (f a)) => ArrowState s (f a) where
	fetch = up fetch
	store = up store

