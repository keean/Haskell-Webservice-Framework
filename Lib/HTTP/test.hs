module Main(main) where

import System.IO
import Network
import Network.Socket

main :: IO ()
main = do
	hPutStr stderr (show $ (\(PortNum z) -> z) p)
	where
	
		p = PortNum 8888
