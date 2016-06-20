module Main where

import Numeric

main :: IO ()
main = do
	s <- getLine
	case readHex s of
		((len,_):_) -> putStr $ showInt len "\n"
		_ -> putStr "\n"

