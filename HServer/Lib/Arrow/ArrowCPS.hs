{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- ArrowIO

module Lib.Arrow.ArrowCPS(ArrowCPS(..)) where

import Control.Arrow

class (Arrow a,ArrowApply a) => ArrowCPS ans a where
	jump :: a (a c ans,c) z
{-
	callcc :: ArrowCPS ans x => 
	
-- ArrowIO

module Lib.Arrow.CPSFunctor(CPSFunctor(..)) where

import Control.Arrow
import Lib.Arrow.ArrowFunctor
import Lib.Arrow.ArrowCPS

--------------------------------------------------------------------
-- State Functor

newtype CPSFunctor ans a b c = CPS ((a c ans) -> (a b ans))

instance (Arrow a,Arrow (CPSFunctor ans a)) => ArrowFunctor (CPSFunctor ans) a where
	up f = CPS $ \k -> f >>> k
	down (CPS f) = f returnA 

instance ArrowApply a => Arrow (CPSFunctor ans a) where
	arr f = up $ arr f
	CPS f >>> CPS g = CPS $ \k -> f (g k)
	first (CPS f) = CPS $ \k -> arr (\(b,d) -> (f (arr (\c -> (c,d)) >>> k),b)) >>> app)

instance (ArrowApply a,ArrowChoice a) => ArrowChoice (CPSFunctor ans a) where
	left (CPS f) = CPS $ \k -> left (f k)

instance (ArrowApply a,ArrowZero a) => ArrowZero (CPSFunctor ans a) where
	zeroArrow = CPS $ \k -> zeroArrow k

instance (ArrowApply a ,ArrowPlus a) => ArrowPlus (CPSFunctor ans a) where
	CPS f <+> CPS g = CPS $ \k -> (f k) <+> (g k)

instance ArrowApply a => ArrowApply (CPSFunctor ans a) where
	app (CPS f) = CPS $ \k -> (f k) >>> app
-}
