{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.MonadControl(MonadControl(..)) where

import Control.Monad

------------------------------------------------------------------------------

class (Monad m,MonadPlus m) => MonadControl m where
	once :: m a -> m a
	(?) :: m a -> m a -> m a
	m ? n = once (m `mplus` n)

instance MonadControl Maybe where
	once = id

instance MonadControl [] where
	once = foldr (\a _ -> return a) mzero

