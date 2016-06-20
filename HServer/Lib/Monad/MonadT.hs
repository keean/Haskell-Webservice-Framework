{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.MonadT(MonadT(..),Runnable(..)) where

import Control.Monad hiding (guard)
import Lib.Arrow.Runnable

------------------------------------------------------------------------------

-- class Runnable m n where
--  	run :: m -> n 

instance Runnable (m a) (m a) where
	run = id

instance Runnable (s -> m a) (s -> m a) where
	run = id

class (Monad m,Monad (t m)) => MonadT t m where
	up :: m a -> t m a
	up1 :: (m a -> m a) -> t m a -> t m a
	up2 :: (m a -> (b -> m a) -> m a) -> t m a -> (b -> t m a) -> t m a
	up3 :: (m a -> (a -> m b) -> (a -> m c) -> m c) -> t m a -> (a -> t m b) -> (a -> t m c) -> t m c
	down :: t m a -> m a
	up1 = undefined
	up2 = undefined
	up3 = undefined

instance (Monad m,Monad n,MonadT t m,Runnable (m a) (n a)) => Runnable (t m a) (n a) where
	run = run . down

instance (Monad m,MonadT t m,Monad (t m)) => Runnable (t m a) (m a) where
	run = down 

