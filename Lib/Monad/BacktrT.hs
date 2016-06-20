{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--      Polymorphic monadic consumer based parser.

module Lib.Monad.BacktrT(BacktrT(..)) where

import Control.Monad hiding (guard)
--import Control.Monad.Except
import Lib.Monad.MonadT
import Lib.Monad.MonadControl
import Lib.Arrow.Runnable
import GHC.Base

------------------------------------------------------------------------------

type Cps a r = (a -> r) -> r
type Endo r = r -> r

newtype BacktrT r m a = BT {bt :: Cps a (Endo (m r))}

instance (Applicative (BacktrT r m), Monad m) => Monad (BacktrT r m) where
    {-# INLINE return #-}
    return a = BT $ \k -> k a 
    {-# INLINE (>>=) #-}
    (BT m) >>= f = BT $ \k -> m (\a -> bt (f a) k)

instance (GHC.Base.Alternative (BacktrT r m), Monad m) => MonadPlus (BacktrT r m) where
    {-# INLINE mzero #-}
    mzero = BT $ \_ -> id
    {-# INLINE mplus #-}
    mplus (BT m) (BT n) = BT $ \k -> m k . n k

instance (Applicative (BacktrT r m), MonadPlus m) => MonadT (BacktrT r) m where
    {-# INLINE up #-}
    up m = BT $ \k f -> (m >>= \a -> k a mzero) `mplus` f
    {-# INLINE down #-}
    down = undefined

instance (MonadPlus m,MonadT (BacktrT r) m,Runnable (m r -> m r) (n r -> n r)) => Runnable (BacktrT r m r) (n r -> n r) where
    run = run . (\(BT m) k -> m (\a f -> return a `mplus` f) k)

instance (MonadPlus m,MonadT (BacktrT r) m) => Runnable (BacktrT r m r) (m r -> m r) where
    run = \(BT m) k -> m (\a f -> return a `mplus` f) k

instance (Alternative (BacktrT r m), Monad m) => MonadControl (BacktrT r m) where
    {-# INLINE once #-}
    once (BT m) = BT $ \k f -> m (\a _ -> k a f) f 

