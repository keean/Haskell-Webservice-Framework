{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- ArrowIO

module Lib.Arrow.ArrowMaybe(ArrowMaybe(..)) where

import Control.Arrow

class Arrow a => ArrowMaybe a where
	errorA :: a b c
	
