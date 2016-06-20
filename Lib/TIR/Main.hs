{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved. 
------------------------------------------------------------------------------

import Control.Exception as Exception
import Numeric
--import Data.Char

import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic
--import Lib.TIR.Peano as Peano
import Lib.TIR.HList
import Lib.TIR.HArray
--import Lib.TIR.HType
import Lib.TIR.HTypeGHC
import Lib.TIR.HRecord
--import Lib.TIR.HRecordGHC

------------------------------------------------------------------------------
-- for some reason we cant import these definitions in ghc 6.3 

{-
infixr 1 :*:
type a :*: r = HCons a r

infixr 1 .*.
(.*.) :: HList l => e -> l -> HCons e l
e .*. l = HCons e l

infixr 1 :#:
type a :#: r = HCons (HProxy a) r
-}

{-
infixr 1 .*.
(.*.) :: HList r => a -> r -> HCons a r
a .*. r = HCons a r
-}

------------------------------------------------------------------------------

data Test1 = Test1 deriving Show
data Test2 = Test2 deriving Show
data Test3 = Test3 deriving Show
data Test4 = Test4 deriving Show
data Test5 = Test5 deriving Show

{-
myTIR :: Test1 :#: Test2 :#: Test3 :#: Test4 :#: HNil
myTIR = tir

testr :: HRecord (Test1 :#: Test2 :#: Test3 :#: Test4 :#: HNil) 
                      (String :*: String :*: String :*: Int :*: HNil)
testr = Record myTIR ("aaa" .*. "bbb" .*. "ccc" .*. (1 :: Int) .*. HNil)

test2 :: HRecord (Test1 :#: Test2 :#: Test3 :#: Test4 :#: HNil) 
                      (String :*: String :*: String :*: Int :*: HNil)
test2 = record myTIR ("ddd" .*. "bbb" .*. "ccc" .*. (1 :: Int) .*. HNil)
-}

test3 :: (Test1 :=: Maybe String :*: Test2 :=: Maybe Int :*: HNil)
test3 = (Test1 .=. Just "aaa" .*. Test2 .=. Nothing .*. HNil)

------------------------------------------------------------------------------

type MyHList = String :*: String :*: Float :*: Int :*: HNil

hlst :: MyHList
hlst = "aaa" .*. "bbb" .*. 1.3 .*. 22 .*. HNil

hlst1 :: Float :*: String :*: Int :*: HNil
hlst1 = 1.3 .*. "bbb" .*. 22 .*. HNil

hlst2 :: Float :*: String :*: Int :*: HNil
hlst2 = 1.3 .*. "ccc" .*. 22 .*. HNil

hlst3 :: Float :*: Float :*: Int :*: HNil
hlst3 = 1.3 .*. 1.3 .*. 22 .*. HNil

hlst4 :: Float :*: Float :*: Int :*: HNil
hlst4 = 1.3 .*. 2.4 .*. 22 .*. HNil

hlst5 :: String :*: Float :*: Int :*: HNil
hlst5 = "bbb" .*. 1.3 .*. 22 .*. HNil

hlst6 :: String :*: Float :*: Int :*: HNil
hlst6 = "ccc" .*. 1.3 .*. 22 .*. HNil

hlst7 :: Float :*: Int :*: HNil
hlst7 = 2.3 .*. 33 .*. HNil

hlst8 :: Char :*: Double :*: Bool :*: HNil
hlst8 = 'a' .*. 4.4 .*. False .*. HNil

hlst9 :: Int :*: Float :*: String :*: String :*: HNil
hlst9 = 22 .*. 1.3 .*. "bbb" .*. "aaa" .*. HNil

hlstA :: Int :*: Float :*: String :*: String :*: HNil
hlstA = 23 .*. 2.3 .*. "bbb" .*. "aaa" .*. HNil

hlstB :: String :*: Char :*: Float :*: Int :*: HNil
hlstB = "aaa" .*. 'b' .*. 1.3 .*. 22 .*. HNil
{-
hfmTest :: HFMap
    (String :*: Char   :*: Float :*: Int :*: HNil)
    (String :*: String :*: Float :*: Int :*: HNil)
hfmTest = HFMap {
    hKeys = undefined,
    hValues = "aaa" .*. "bbb" .*. 1.3 .*. 22 .*. HNil
}
-}

i10 :: Int
i10 = 10

i22 :: Int
i22 = 22

d1 :: Double
d1 = 1.0

check :: (Monad p,Logic.Eq m n o,Logic.Modal o) => Int -> m -> n -> p [Int]
check i m n = if Logic.reflectBool (m `Logic.eq` n)
    then return []
    else return [i]

tHead :: HCons e l -> e
tHead (HCons e _) = e

tTail :: HCons e l -> l
tTail (HCons _ l) = l

tTest :: HList l => HCons a (HCons b (HCons c l)) -> c
tTest l = hHead (hTail (hTail l))

mapExceptionIO :: (IOException -> IOException) -> IO a -> IO a
mapExceptionIO f io = Exception.catch io (\e -> throw $ f e)

main :: IO ()
main = mapExceptionIO (\x -> userError $ showString "in main: " $ show x) $ do
    l <- dot tests
    putStrLn $ (showString "The following tests failed: " . shows (concat l)) ""
    -- print (hrCons testr ((undefined,"zzz")::(Test5,String)))
    print (hAfterFst hlst8 (undefined :: Double))
    print (hDeleteFst hlst8 (undefined :: Double))
    print (hDeleteFst hlst9 (undefined :: String))
    -- print (hrProject testr (tir :: Test2 :#: Test3 :#: HNil))
    print (hSelectAllByHList hlst ((undefined::String) .*. (undefined::Float) .*. HNil))
    -- print (hDeleteAllByHList hlst ((undefined::String) .*. (undefined::Float) .*. HNil))
    print test3
    print (rLookup test3 Test1)
    print (hHead (hTail (hTail hlst)))
    --print (tTest hlst)
    a <- getLine
    hLookupWith ((fst . head . readDec $ a) :: Int) hlst Tst

data Tst = Tst
instance Show e => Ctrl.Cont Tst e (IO ()) where
    cont Tst e = print e

dot :: [IO [Int]] -> IO [[Int]]
dot (f0:fs) = do
    a0 <- f0
    as <- dot fs
    return (a0:as)
dot _ =     return []

tests :: [IO [Int]]
tests = [
    check 1 (hlst `hContainsType` i10) Logic.NotTrue,
    check 2 (hlst `hContainsType` i22) Logic.True,
    check 3 (hlst `hContainsType` d1) Logic.AllFalse,
    check 4 (HNil `hContainsType` d1) Logic.AllFalse{-,
    check 5 (hlst `hSublist` hlst1) Logic.True,
    check 6 (hlst `hSublist` hlst2) Logic.NotTrue,
    check 7 (hlst `hSublist` hlst3) Logic.True,
    check 8 (hlst `hSublist` hlst4) Logic.NotTrue,
    check 9 (hlst `hSublist` HNil) Logic.AllTrue,
    check 10 (HNil `hSublist` hlst) Logic.AllFalse,
    check 11 (HNil `hSublist` HNil) Logic.AllTrue-},
    check 12 (hlst `hSubType` hlst1) Logic.AllFalse,
    check 13 (hlst `hSubType` hlst5) Logic.True,
    check 14 (hlst `hSubType` hlst6) Logic.NotTrue,
    check 15 (hlst `hSubType` HNil) Logic.AllTrue,
    check 16 (HNil `hSubType` hlst) Logic.AllFalse,
    check 17 (HNil `hSubType` HNil) Logic.AllTrue{-,
    check 18 (hlst `hDisjunct` hlst6) Logic.False,
    check 19 (hlst `hDisjunct` hlst7) Logic.NotFalse,
    check 20 (hlst `hDisjunct` hlst8) Logic.AllTrue,
    check 21 (hlst `hDisjunct` HNil) Logic.AllTrue,
    check 22 (HNil `hDisjunct` hlst) Logic.AllTrue,
    check 23 (HNil `hDisjunct` HNil) Logic.AllTrue,
    check 24 (hlst `hConjunct` hlst6) Logic.NotTrue,
    check 25 (hlst `hConjunct` hlst7) Logic.AllFalse,
    check 26 (hlst `hConjunct` hlst8) Logic.AllFalse,
    check 27 (hlst `hConjunct` hlst9) Logic.True,
    check 28 (hlst `hConjunct` hlstA) Logic.NotTrue,
    check 29 (hlst `hConjunct` HNil) Logic.AllFalse,
    check 30 (HNil `hConjunct` hlst) Logic.AllFalse,
    check 31 (HNil `hConjunct` HNil) Logic.AllTrue,
    check 32 (HNil `hIdentical` HNil) Logic.AllTrue,
    check 33 (hlst `hIdentical` hlst) Logic.True,
    check 34 (hlst1 `hIdentical` hlst2) Logic.NotTrue,
    check 35 (hlst `hIdentical` hlst1) Logic.AllFalse,
    check 36 (hlst5 `hIdentical` (hAssignTP hlst6 zero "bbb")) Logic.True,
    check 37 (hlst `hIdentical` (hAssignTC hlstB one "bbb")) Logic.True-},
    check 38 (hlst `hSubType` hlst1) Logic.AllTrue,
    check 39 (hlst `hSubType` hlst2) Logic.AllTrue,
    check 40 (hlst `hSubType` hlst3) Logic.AllTrue,
    check 41 (hlst `hSubType` hlst4) Logic.AllTrue,
    check 42 (hlst `hSubType` HNil) Logic.AllTrue,
    check 43 (HNil `hSubType` hlst) Logic.AllFalse,
    check 44 (HNil `hSubType` HNil) Logic.AllTrue,
    check 45 (hlst7 `hContainsType` (undefined::Double)) Logic.AllFalse,
    check 46 (hlst8 `hContainsType` (undefined::Double)) Logic.AllTrue{-,
    
    check 100 (hlst `hIdentical` (hReverse (hReverse hlst))) Logic.True,
    check 101 (hSingleton (hrLookup testr Test3) `hIdentical` hSingleton "ccc") Logic.True-}
    ]


