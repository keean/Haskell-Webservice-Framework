{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.EndoT(EndoT(..)) where

import Control.Monad hiding (guard)
import Lib.Monad.MonadT
import Lib.Monad.MonadControl
import Lib.Monad.MonadSoln

------------------------------------------------------------------------------

newtype EndoT m a = ET (m a -> m a)

instance MonadPlus m => Monad (EndoT m) where
 	(ET m) >>= k = ET (\f -> m mzero >>= \a -> ((\(ET x) -> x) (k a)) mzero `mplus` f)
 	return a = ET (\f -> return a `mplus` f)

instance MonadPlus m => MonadPlus (EndoT m) where
 	mzero = ET (id)
 	(ET m) `mplus` (ET n) = ET (n . m)

instance MonadPlus m => MonadT EndoT m where
	up m = ET (\f -> m `mplus` f)
 	down (ET m) = m mzero

instance MonadControl m => MonadControl (EndoT m) where
	once (ET m) = ET $ \f -> once (m mzero) `mplus` f

instance MonadSoln m => MonadSoln (EndoT m) where
	solutions (ET m) = ET $ \f -> solutions (m mzero) `mplus` f

