{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- ArrowIO

module Lib.Arrow.Kleisli() where

import Control.Monad
import Control.Arrow
import Lib.Arrow.Runnable

-- toKleisli :: (a -> m b) -> (Kleisli m) a b
-- toKleisli m = Kleisli m

fromKleisli :: (Kleisli m a b) -> (a -> m b)
fromKleisli (Kleisli m) = m

instance (Arrow (Kleisli m),Monad m,Runnable (b -> m c) z) => Runnable (Kleisli m b c) z where
	run = run . \a -> fromKleisli a
