
-- ArrowIO

module Lib.Arrow.CPSFunctor(CPSFunctor(..)) where

import Control.Arrow
import Lib.Arrow.Runnable
import Lib.Arrow.ArrowFunctor
-- import Lib.Arrow.ArrowCPS

newtype CPSFunctor ans a b c = CPS { runCPS :: (a c ans) -> (a b ans) }

instance (Arrow a,Arrow (CPSFunctor ans a),ArrowApply a) => ArrowFunctor (CPSFunctor ans) a where
	up f = CPS $ \k -> f >>> k
	down (CPS f) = f (arr (\_ -> undefined)) >>> arr (\_ -> undefined)

instance (ArrowFunctor (CPSFunctor ans) a,Runnable (a b ans) z) => Runnable ((CPSFunctor ans a) b ans) z where
	run = run . (\(CPS f) -> f returnA)

instance ArrowApply a => Arrow (CPSFunctor ans a) where
	arr f = up $ arr f
	CPS f >>> CPS g = CPS $ \k -> f (g k)
	first (CPS f) = CPS $ \k -> arr (\(b,d) -> (f (arr (\c -> (c,d)) >>> k),b)) >>> app

instance (ArrowApply a,ArrowChoice a) => ArrowChoice (CPSFunctor ans a) where
	left (CPS f) = CPS $ \k -> arr (\z -> case z of
		Left b -> (arr (\() -> b) >>> f (arr Left >>> k),())
		Right d -> (arr (\() -> Right d) >>> k,())) >>> app

instance (ArrowApply a,ArrowZero a) => ArrowZero (CPSFunctor ans a) where
	zeroArrow = CPS $ \_ -> zeroArrow

instance (ArrowApply a ,ArrowPlus a) => ArrowPlus (CPSFunctor ans a) where
	CPS f <+> CPS g = CPS $ \k -> (f k) <+> (g k)

instance ArrowApply a => ArrowApply (CPSFunctor ans a) where
	app = CPS $ \k -> arr (\(CPS f,x) -> (f k,x)) >>> app

------------------------------------------------------------------------------

-- class Arrow (t ans) => ArrowCPS t ans where
-- 	jump :: t ans a (a c ans,c) z

-- jump :: ArrowApply a => CPSFunctor ans a (a c ans,c) z

-- instance ArrowCPS (CPSFunctor a) ans
-- jump = CPS $ \_ -> app

callcc :: ArrowApply a => (a c ans -> CPSFunctor ans a b c) -> CPSFunctor ans a b c
callcc f = CPS $ \k -> let CPS g = f k in g k
