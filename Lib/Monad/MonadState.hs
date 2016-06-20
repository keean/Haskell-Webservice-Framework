{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.MonadState(MonadState(..)) where

--import Control.Monad hiding (guard)
import Lib.Monad.MonadT

------------------------------------------------------------------------------

class Monad m => MonadState st m where
    update :: (st -> st) -> m st
    getState :: m st
    setState :: st -> m ()

instance {-# OVERLAPPABLE #-} (MonadState st m,MonadT t m) => MonadState st (t m) where
    update = up . update
    setState = up . setState
    getState = up $ getState

