{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Main where

import Control.Monad.Error
import System.Posix.Signals

import Lib.Monad.MonadIO
import Lib.DBC.Types
import Lib.DBC.HIODBC
												   
db :: OdbcConnection
db = odbcConnection { odbcDsn = "testdb" }
																			   
main :: IO ()
main = do
	installHandler sigPIPE Ignore Nothing
	odbcConnect db sqlMain
																														    
sqlMain :: SqlHandle ()
sqlMain = do
	(sqlDo "DROP TABLE test3")
		`ioCatch` (\e -> do
			ioPutStrLn (show e)
			ioPutStrLn "Warning: table 'test3' not dropped")
	sqlDo "CREATE TABLE test3 (id serial primary key, value int, name text)"
	sqlDo "INSERT INTO test3 (value,name) VALUES(1,'test1')"
	sqlDo "INSERT INTO test3 (value,name) VALUES(2,'test2')"
	sqlDo "INSERT INTO test3 (value,name) VALUES(3,'test3')"
	(cols,rows) <- sqlQuery "SELECT * FROM test3"
	ioPrint cols
	ioPrint rows

