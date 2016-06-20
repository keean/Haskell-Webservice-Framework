{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- ArrowIO

module Lib.Arrow.ArrowFunctor(ArrowFunctor(..)) where

import Control.Arrow
import Lib.Arrow.Runnable

--------------------------------------------------------------------
-- classes for arrow reduction.

class (Arrow a,Arrow (f a)) => ArrowFunctor f a where
	up :: a b c -> (f a) b c
	down :: (f a) b c -> a b c

instance (ArrowFunctor f x,Runnable (x b c) z) => Runnable (f x b c) z where
	run = run . down

instance Monad m => Runnable (b -> m c) (b -> m c) where
	run = id
