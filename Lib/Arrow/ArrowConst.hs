

module Lib.Arrow.ArrowConst(ArrowConst(..)) where

import Control.Arrow

class Arrow a => ArrowConst s a where
	const :: a b s
