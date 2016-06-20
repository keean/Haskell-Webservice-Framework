{-# OPTIONS -fno-implicit-prelude -fglasgow-exts #-}
-- generalised monad type supporting continuations

-- M o p a -> (a -> M p q b) -> M o q b

module Lib.HyperMonad(HMonad(..),HMonadPlus(..)) where

import Prelude hiding ((>>=),return)

class HMonad m where
	(>>=) :: m o p a -> (a -> m p q b) -> m o q b
	return :: a -> m o o a

class HMonadPlus m where
	mzero :: m o o a
	mplus :: m o p a -> m o p a -> m o p a

class (HMonad (t m),HMonad m) => HMonadT t m where
	up :: m o p a -> t m o p a
	down :: t m o p a -> m o p a
	
newtype ContT m b o a = CT { ct :: (a -> m p q b) -> m o q b}

instance HMonad m => HMonad (ContT m) where
	return a = CT $ \k -> k a
	(CT m) >>= n = CT $ \k -> m (\a -> ct (n a) k)

-- instance HMonadPlus m => HMonadPlus (ContT m) where
-- 	mzero = CT $ \_ -> mzero
-- 	(CT m) `mplus` (CT n) = CT $ \k -> m k `mplus` n k

-- instance HMonad m => HMonadT ContT m where
-- 	up m = CT $ \k -> m >>= k
-- 	down m = CT $ m undefined >>= \(_,a) -> return a
