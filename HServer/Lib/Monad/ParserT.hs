{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.ParserT(ParserT(..)) where

import Control.Monad hiding (guard)
import Control.Monad.Error
import Lib.Monad.MonadT
import Lib.Monad.MonadState
import Lib.Monad.MonadParser
import Lib.Monad.MonadControl
import Lib.Arrow.Runnable

------------------------------------------------------------------------------
-- An continuation passing endomorphic parser 

type Cps a r = (a -> r) -> r
type Endo r = r -> r

newtype ParserT r tok m a = PT (Cps a ([tok] -> Endo (m r)))

instance Monad m => Functor (ParserT r tok m) where
	fmap g (PT m) = PT $ \k -> m (\a s f -> k (g a) s f)

instance Monad m => Monad (ParserT r tok m) where
	{-# INLINE return #-}
	return a = PT $ \k -> k a 
	{-# INLINE (>>=) #-}
	(PT m) >>= f = PT $ \k -> m (\a -> (\(PT x) -> x) (f a) k)

instance Monad m => MonadPlus (ParserT r tok m) where
	{-# INLINE mzero #-}
  	mzero = PT $ \_ _ f -> f
	{-# INLINE mplus #-}
 	mplus (PT m) (PT n) = PT $ \k s -> m k s . n k s

instance MonadPlus m => MonadT (ParserT r tok) m where
	{-# INLINE up #-}
  	up m = PT $ \k s f -> (m >>= \a -> k a s mzero) `mplus` f
	{-# INLINE down #-}
 	down = undefined

instance (MonadPlus m,MonadT (ParserT r tok) m,Runnable ([tok] -> m ([tok],r)) ([tok] -> n ([tok],r)))
		=> Runnable (ParserT ([tok],r) tok m r) ([tok] -> n ([tok],r)) where
	run = run . (\(PT m) t -> m (\a t' f -> return (t',a) `mplus` f) t mzero)

instance (MonadPlus m,MonadT (ParserT r tok) m)
		=> Runnable (ParserT ([tok],r) tok m r) ([tok] -> m ([tok],r)) where
	run = (\(PT m) t -> m (\a t' f -> return (t',a) `mplus` f) t mzero)

instance Monad m => MonadState [tok] (ParserT r tok m) where
	{-# INLINE update #-}
	update st = PT $ \k s -> k s ((st s) `asTypeOf` s)
	setState st = PT $ \k _ -> k () st
	getState = PT $ \k s -> k s s
	

instance Monad m => MonadParser tok (ParserT r tok m) where
	{-# INLINE item #-}
	item = PT $ \k s -> case s of
		[] -> id
		(a:x) -> k a x

instance (MonadPlus (t m),MonadParser tok m,MonadT t m) => MonadParser tok (t m) where
	item = up item

instance Monad m => MonadControl (ParserT r tok m) where
	{-# INLINE once #-}
	once (PT m) = PT $ \k s f -> m (\a s' _ -> k a s' f) s f 

