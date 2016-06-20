{-# OPTIONS -fglasgow-exts #-}

-- server.hs: Copyright (C)2002 Keean Schupke
--
--		HyperServer main module

module Main(main) where

import Socket
import FiniteMap
import Directory
import Exception
import IO

import Lib.Linker.Linker
import Lib.Parser.Parser
import Lib.Server.Types

import Server.HServer

import Modules.Issues.App
import Modules.Admin.App

main :: IO ()
main = do -- load libraries and server
	socket <- listenOn (PortNumber 8888)
	issues <- Modules.Issues.App.initialise
	admin <- Modules.Admin.App.initialise
	startApp issues
	startApp admin
	serverMain socket (listToFM [issues,admin])

----------------------------------------------------------------

startAll :: [IO Application] -> IO [Application]
startAll  [] = return []
startAll (fn:fns) = do
	j0 <- fn
	startApp j0 -- call start method
	js <- startAll fns
	return (j0:js)

stopAll :: [Application] -> IO ()
stopAll [] = return ()
stopAll (fn:fns) = do
	stopApp fn
	js <- stopAll fns
	return ()
	
getModules :: String -> [String] -> IO [(String,String)]
getModules _ [] = return []
getModules dir (m0:ms) = do
	case m0 of
		"." -> getModules dir ms
		".." -> getModules dir ms
		"makefile" -> getModules dir ms
		_ -> case dir++('/':m0) of
			m -> do
				files <- getDirectoryContents m
				case filterFiles m files of
					[] -> getModules dir ms
					objs -> do
						more <- getModules dir ms
						return (objs++more)

-- file list -> initialiser list
linkObjs :: [(String,String)] -> IO [IO Application]
linkObjs [] = return []
linkObjs ((f,_):fs) = do
	link <- linkerLookupSymbol f "initialise"
	case link of
		Nothing -> linkObjs fs
		Just (obj :: IO Application) -> do
			objs <- linkObjs fs
			return (obj:objs)
	`Exception.catch` (\e -> do
		hPutStr stderr (shows e "\n")
		objs <- linkObjs fs
		return objs)

-- load files from list
loadObjs :: [(String,String)] -> IO ()
loadObjs [] = return ()
loadObjs ((_,f):fs) = do
	linkerLoadObj (showString f ".o")
	`Exception.catch` (\e -> hPutStr stderr (shows e "\n"))
	`finally` (loadObjs fs)

loadArch :: [String] -> IO ()
loadArch [] = return ()
loadArch (f:fs) = do
	linkerLoadObj (showString f ".o")
	`Exception.catch` (\e -> hPutStr stderr (shows e "\n"))
	`finally` (loadArch fs)

unloadObjs :: [(String,String)] -> IO ()
unloadObjs [] = return ()
unloadObjs ((_,f):fs) = do
	linkerUnloadObj (showString f ".o")
	`Exception.catch` (\e -> hPutStr stderr (shows e "\n"))
	`finally` (unloadObjs fs)

-- file list -> object file list
filterFiles :: String -> [String] -> [(String,String)]
filterFiles _ [] = []
filterFiles path (f:fs) = case parseResult parseObjFile f of
	Ok p _ -> (p,path++('/':p)):filterFiles path fs
	_ -> filterFiles path fs

-- parser accepts files ending in .o
parseObjFile :: Parser String
parseObjFile = do
	a <- untilParser (do string ".o"; matchEnd) char
	return a

