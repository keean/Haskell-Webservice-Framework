{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.MonadParser(MonadParser(..)) where

import Control.Monad hiding (guard)

------------------------------------------------------------------------------

class MonadPlus m => MonadParser tok m where
	item :: m tok

