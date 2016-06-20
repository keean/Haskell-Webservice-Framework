{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- ArrowIO

module Lib.Arrow.Runnable(Runnable(..)) where

import Lib.Monad.MonadT

--------------------------------------------------------------------
-- classes for generalised reduction.

class Runnable x y where
   run :: x -> y

instance Monad m => Runnable (m a) (m a) where
    run = id

instance Monad m => Runnable (s -> m a) (s -> m a) where
    run = id

instance {-# OVERLAPPABLE #-} (Monad m, Monad n, MonadT t m, Runnable (m a) (n a)) => Runnable (t m a) (n a) where
    run = run . down

instance {-# OVERLAPPING #-} (Monad m, MonadT t m, Monad (t m)) => Runnable (t m a) (m a) where
    run = down


