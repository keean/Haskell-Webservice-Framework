{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- ArrowIO

module Lib.Arrow.MaybeFunctor(MaybeFunctor(..)) where

import Control.Arrow
import Lib.Arrow.Runnable
import Lib.Arrow.ArrowMaybe
import Lib.Arrow.ArrowFunctor

--------------------------------------------------------------------
-- Maybe Functor

newtype MaybeFunctor a b c = MF { runMF :: a b (Maybe c) }

instance (Arrow a,Arrow (MaybeFunctor a)) => ArrowFunctor MaybeFunctor a where
	up f = MF (f >>> arr Just)
	down (MF f) = f >>> arr (\x -> case x of
		Just a -> a
		Nothing -> undefined)

instance (ArrowFunctor MaybeFunctor x,Runnable (x b (Maybe c)) z) => Runnable (MaybeFunctor x b c) z where
	run = run . (\(MF f) -> f)

instance ArrowChoice a => Arrow (MaybeFunctor a) where
	arr f = up (arr f)
	MF f >>> MF g = MF $ f >>> arr (\z -> case z of
		Just c -> Left c
		Nothing -> Right Nothing) >>> (g ||| arr id)
	first (MF f) = MF $ first f >>> arr (\(c',d) -> case c' of
		Just c -> Just (c,d)
		Nothing -> Nothing)
	
instance ArrowChoice a => ArrowZero (MaybeFunctor a) where
	zeroArrow = MF $ arr (\_ -> Nothing)

instance ArrowChoice a => ArrowPlus (MaybeFunctor a) where
	(MF f) <+> (MF g) = MF $ (f &&& arr id) >>> arr (\(c',b) -> case c' of
		Just _ -> Left c'
		Nothing -> Right b) >>> (arr id ||| g)

instance ArrowChoice a => ArrowChoice (MaybeFunctor a) where
	left (MF f) = MF $ left f >>> arr (\z -> case z of
		Left (Just x) -> Just (Left x)
		Left Nothing -> Nothing
		Right y -> Just (Right y))

instance (ArrowChoice a,ArrowApply a) => ArrowApply (MaybeFunctor a) where
	app = MF $ arr (\(MF f,b) -> (f,b)) >>> app

instance ArrowChoice a => ArrowMaybe (MaybeFunctor a) where
	errorA = MF $ arr (\_ -> Nothing)

instance (ArrowMaybe a,ArrowFunctor f a,Arrow (f a)) => ArrowMaybe (f a) where
	errorA = up errorA


