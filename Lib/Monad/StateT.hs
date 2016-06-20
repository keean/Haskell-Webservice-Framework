{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--      Polymorphic monadic consumer based parser.

module Lib.Monad.StateT(StateT(..)) where

-- import Control.Monad (Monad(..), MonadPlus(..)) --(guard)
import Lib.Monad.MonadT
import Lib.Monad.MonadState
import Lib.Monad.MonadControl
import Lib.Monad.MonadSoln
import Lib.Arrow.Runnable
import GHC.Base

------------------------------------------------------------------------------

newtype StateT st m a = ST { runST :: st -> m (st, a) }

instance Functor m => Functor (StateT st m) where
    fmap f m = ST $ \s -> fmap (\(s', a) -> (s', f a)) $ runST m s

instance (Functor m, Monad m) => Applicative (StateT st m) where
    pure x = ST $ \s -> return (s, x)
    mf <*> mx = ST $ \s -> do
        (s', f) <- runST mf s
        (s'', x) <- runST mx s'
        return (s'', f x)

instance (Functor m, MonadPlus m) => Alternative (StateT st m) where
    empty = ST $ \_ -> mzero
    ST m <|> ST n = ST $ \s -> m s `mplus` n s

instance Monad m => Monad (StateT st m) where
    m >>= k = ST $ \s -> do
        (s', a) <- runST m s 
        runST (k a) s'
    return a = ST $ \s -> return (s,a)

instance MonadPlus m => MonadPlus (StateT st m) where
    mzero = ST $ \_ -> mzero
    ST m `mplus` ST n = ST $ \s -> m s `mplus` n s

instance (MonadState st (StateT st m),Monad m) => MonadT (StateT st) m where
    up m = ST $ \s -> do
        a <- m
        return (s,a)
    up1 f m = ST $ \s -> do
        a <- f (downST m s)
        return (s,a)
    up2 f m n = ST $ \s -> do
        a <- f (downST m s) (downST' n s)
        return (s,a)
    up3 f m n o = ST $ \s -> do
        a <- f (downST m s) (downST' n s) (downST' o s)
        return (s,a)
    down (ST m) = do
        (_,a) <- m undefined
        return a

downST :: Monad m => StateT st m a -> (st -> m a)
downST m = \st -> do
    (_,a) <- runST m st
    return a

downST' :: Monad m => (b -> StateT st m a) -> (st -> b -> m a)
downST' m = \st b -> do
    (_,a) <- runST (m b) st
    return a
    

instance {-# OVERLAPPABLE #-} (MonadState st (StateT st m),Monad m,Monad n,Runnable (st -> m s) (st -> n s)) => Runnable (StateT st m s) (st -> n s) where
    run = run . downST 

instance {-# OVERLAPPING #-} (MonadState st (StateT st m),Monad m) => Runnable (StateT st m s) (st -> m s) where
    run = downST

instance {-# OVERLAPPING #-} (Applicative (StateT st m), Monad m) => MonadState st (StateT st m) where
    update st = ST $ \s -> return (st s,s)
    setState st = ST $ \_ -> return (st,())
    getState = ST $ \s -> return (s,s)

--instance {-# OVERLAPPABLE #-} (MonadState st m,MonadT t m) => MonadState st (t m) where
--    update = up . update
--    setState = up . setState
--    getState = up $ getState

instance (MonadState st (StateT st m),MonadControl m) => MonadControl (StateT st m) where
    once (ST m) = ST $ \s -> once (m s)

instance (MonadState st (StateT st m),MonadSoln m) => MonadSoln (StateT st m) where
    solutions (ST m) = ST $ \s -> solutions (m s) >>= \x -> return (s,map snd x)
