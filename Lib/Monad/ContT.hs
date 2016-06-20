{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--      Polymorphic monadic consumer based parser.

module Lib.Monad.ContT(ContT(..)) where

import Control.Monad hiding (guard)
import Lib.Monad.MonadT
import Lib.Arrow.Runnable
import Lib.Monad.MonadContinuation
import GHC.Base

------------------------------------------------------------------------------

newtype ContT r m a = CT { runCT :: (a -> m r) -> m r }

instance (Applicative (ContT r m), Monad m) => Monad (ContT r m) where
    (CT m) >>= k = CT $ \kappa -> m (\a -> runCT (k a) kappa)
    return a = CT $ \kappa -> kappa a
 
instance (GHC.Base.Alternative (ContT r m), MonadPlus m) => MonadPlus (ContT r m) where
    mzero = CT $ \_ -> mzero
    (CT m) `mplus` (CT n) = CT $ \kappa -> m kappa `mplus` n kappa

instance (Monad (ContT r m),Monad m) => MonadT (ContT r) m where
    up m = CT $ \kappa -> m >>= kappa
    down = undefined

instance (MonadT (ContT r) m,Runnable ((r -> m r) -> m r) ((r -> n r) -> n r)) => Runnable (ContT r m r) ((r -> n r) -> n r) where
    run = run . (\(CT m) kappa -> m kappa)

instance MonadT (ContT r) m => Runnable (ContT r m r) ((r -> m r) -> m r) where
    run = (\(CT m) kappa -> m kappa)

instance (Applicative (ContT r m), Monad m) => MonadContinuation (ContT r m) where
    callcc f = CT $ \k -> runCT (f (\a -> CT $ \_ -> k a)) k
