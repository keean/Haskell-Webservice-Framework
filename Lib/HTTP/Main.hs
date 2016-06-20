module Main(main) where

import Data.Char
import System.IO
import Lib.Parser.Parser
import Lib.HTTP.Types
import Lib.HTTP.Parser

main :: IO ()
main = do
	h <- openFile "test.xml" ReadMode
	hSetBuffering h (BlockBuffering Nothing)
	hSetBuffering stdout (BlockBuffering Nothing)
	input <- hGetContents h
	case parse httpRequest input of
		Just req -> print req
		Nothing -> print "Nothing"
