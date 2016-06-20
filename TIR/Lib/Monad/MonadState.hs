{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.MonadState where

import Control.Monad hiding (guard)

------------------------------------------------------------------------------

class Monad m => MonadState st m where
	update :: (st -> st) -> m st
	getState :: m st
	setState :: st -> m ()

