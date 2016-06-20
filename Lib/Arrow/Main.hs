
-- ArrowIO

module Main(test,main) where

import Control.Monad
import Control.Arrow
import Lib.Arrow.Runnable
import Lib.Arrow.Kleisli
import Lib.Arrow.ArrowFunctor
import Lib.Arrow.ArrowIO
import Lib.Arrow.IOArrow
import Lib.Arrow.ArrowState
import Lib.Arrow.StateFunctor
import Lib.Arrow.CPSFunctor
import Lib.Arrow.ArrowMaybe
import Lib.Arrow.MaybeFunctor

arrConst :: Arrow a => c -> a b c 
arrConst k = arr $ \_ -> k

arrNull :: Arrow a => a b ()
arrNull = arr $ \_ -> ()

test :: (ArrowApply a,ArrowMaybe a,ArrowState String a,ArrowIO a) => a String String
test = proc x -> do
	arrPutStr -< x
	arrPutStr -< "\n"
	a <- arrGetLine -< ()
	store -< a
	b <- arrGetLine -< ()
	arrPutStr -< b
	arrPutStr -< "\n"
	c <- arrGetLine -< ()
	arrPutStr -< c
	arrPutStr -< "\n"
	d <- fetch -< ()
	arrPutStr -< d
	arrPutStr -< "\n"
	errorA -< ()
	e <- fetch -< ()
	returnA -< e

type TestType s = CPSFunctor s (MaybeFunctor (StateFunctor s (Kleisli IO)))

testArrow :: TestType String String String
testArrow = proc x -> do
	test -< x

-- testLink :: CPSFunctor (TestType String String String,String) (Kleisli IO) String (TestType String String String,String)
-- testLink = arr (\x -> (testArrow,run testArrow x)) 

main :: IO ()
main = do
	(a :: Maybe String) <- run testArrow "test"
	print a
