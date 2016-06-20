{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- ArrowIO

module Lib.Arrow.ArrowState(ArrowState(..)) where

import Control.Arrow

class Arrow a => ArrowState s a where
	const :: a b s
	
