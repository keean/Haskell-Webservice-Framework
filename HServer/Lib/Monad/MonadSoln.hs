{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.MonadSoln(MonadSoln(..)) where

import Control.Monad

------------------------------------------------------------------------------

class MonadPlus m => MonadSoln m where
	solutions :: m a -> m [a]

instance MonadSoln Maybe where
	solutions = return . maybe [] (\a -> [a])

instance MonadSoln [] where
	solutions x = [x]
	
