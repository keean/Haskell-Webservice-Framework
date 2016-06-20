
-- ArrowIO

module Lib.Arrow.ArrowIO(ArrowIO(..)) where

import Control.Arrow

class Arrow a => ArrowIO a where
	arrGetLine :: a () String
	arrPutStr :: a String ()

