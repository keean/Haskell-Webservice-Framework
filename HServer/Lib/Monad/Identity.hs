module Identity(Identity(..)) where

import Control.Monad
import Control.Monad.Fix

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
	fmap f m = Identity (f (runIdentity m))

instance Monad Identity where
	return a = Identity a
	m >>= k  = k (runIdentity m)

instance MonadFix Identity where
	mfix f = Identity (fix (runIdentity . f))
