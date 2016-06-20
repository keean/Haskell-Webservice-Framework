{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Arrow.IOArrow(IOArrow) where

import Control.Arrow
import Lib.Arrow.ArrowIO
import Lib.Arrow.ArrowFunctor

type IOArrow = Kleisli IO

instance ArrowIO (IOArrow) where
    arrGetLine = Kleisli (\_ -> getLine)
    arrPutStr = Kleisli (\a -> putStr a)

instance (ArrowIO a,ArrowFunctor f a,Arrow (f a)) => ArrowIO (f a) where
    arrGetLine = up arrGetLine
    arrPutStr = up arrPutStr
