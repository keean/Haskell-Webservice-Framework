{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Arrow.ArrowState(ArrowState(..)) where

import Control.Arrow
import Lib.Arrow.Runnable

class Arrow a => ArrowState s a where
    update :: (Arrow b,Runnable (b s s) (s -> s)) => a (b s s) s 
    store :: a s ()
    fetch :: a b s
