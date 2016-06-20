{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--      Polymorphic monadic consumer based parser.

module Lib.XML.DomT(DomT(..)) where

--import Data.Functor;
import Control.Monad hiding (guard)
import Lib.Arrow.Runnable
import Lib.Monad.MonadT
import Lib.XML.Types
import Lib.XML.MonadDom
import GHC.Base

------------------------------------------------------------------------------

newtype DomT m a = DT { dt :: (Int,[String],ShowDOM) -> m (a,(Int,[String],ShowDOM)) }

instance Monad m => Functor (DomT m) where
    fmap f (DT m) = DT $ \d -> do
        (x, d') <- m d
        return (f x, d')

instance Monad m => Applicative (DomT m) where
    pure x = DT $ \d -> return (x, d)
    DT mf <*> DT mx = DT $ \d -> do
        (f, d') <- mf d
        (x, d'') <- mx d'
        return (f x, d'')

instance Monad m => Monad (DomT m) where
    return a = DT $ \d -> return (a,d)
    (DT m) >>= n = DT $ \d -> m d >>= \(a,d') -> dt (n a) d'

instance MonadPlus m => Alternative (DomT m) where
    empty = DT $ \_ -> mzero
    (DT m) <|> (DT n) = DT $ \d -> m d `mplus` n d

instance MonadPlus m => MonadPlus (DomT m) where
    mzero = DT $ \_ -> mzero
    (DT a) `mplus` (DT b) = DT $ \d -> a d `mplus` b d

instance Monad m => MonadT DomT m where
    up m = DT $ \d -> m >>= \a -> return (a,d)
    down (DT m) = m (0,[],id) >>= \(a,_) -> return a

instance (Monad m,Runnable (ShowDOM -> m (a,ShowDOM)) (ShowDOM -> n (a,ShowDOM)))
        => Runnable (DomT m a) (ShowDOM -> n (a,ShowDOM)) where
    run = run . \(DT m) -> (\d -> do
        (a,(_,_,d')) <- m (0,[],d)
        return (a,d'))

-- instance (Monad m,Runnable (m a) (n a)) => Runnable (DomT m a) (n a) where
--  run = run . \(DT m) -> do
--      (a,_) <- m (0,[],id)
--      return a

-- instance Monad m => Runnable (DomT m a) (m a) where
--  run = \(DT m) -> do
--      (a,_) <- m (0,[],id)
--      return a

instance Monad m => MonadDom (DomT m) where
    domBegin s = DT $ \(i,t,d) -> return ((),(i+1,s:t,d))
    domEnd = DT $ \(i,t@(t0:ts),d) -> if i>0 then return ((),(i-1,ts,d . \f -> (i-1,ETag t0):f)) else return ((),(i,t,d))
    domElem e = DT $ \(i,t,d) -> return ((),(i,t,d . (\z -> (i,e):z)))
    domPop = DT $ \(i,t,d) -> return (d,(i,t,id))
    (DT m) `domBreak` n = DT $ \d -> m d >>= \(a',d') -> return (n a',d')
    domPush dm = DT $ \(i,t,d) -> return ((),(i,t,d . prepend  i dm))
    domContainer s attr frag = do
        domElem $ STag s (toXml attr)
        domBegin s
        a <- frag
        domEnd
        return a

