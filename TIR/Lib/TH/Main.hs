{-# OPTIONS -fglasgow-exts #-}

module Main where

import TTypeable
{-
import TPrelude
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
-}

-- $("Main" `label` "test")

test :: TypeEq Int Float t => t
test = undefined

test2 :: TypeEq [Maybe Integer] [Maybe Integer] t => t
test2 = undefined

-- test3 :: TypeEq (Test2 Maybe Either) (Test2 IO Either) t => t
-- test3 = undefined

main :: IO ()
main = do
	putStrLn "OK"

