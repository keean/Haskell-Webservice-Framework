
-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--      Polymorphic monadic consumer based parser.

module Lib.Monad.MonadContinuation(MonadContinuation(..)) where

--import Control.Monad

------------------------------------------------------------------------------

class Monad m => MonadContinuation m where
    callcc :: ((a -> m b) -> m a) -> m a
    -- shift :: ((a -> m b) -> m b) -> m a
    -- reset :: m b -> m b
