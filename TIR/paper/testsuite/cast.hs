{-# OPTIONS -fglasgow-exts #-}

{-

This module illustrates heterogeneously typed lists.

-}

module Main where

import Data.Typeable

data Dynamic = forall a. Typeable a => Dynamic a

toDyn :: Typeable a => a -> Dynamic
toDyn = Dynamic

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic x) = cast x

-- Heterogeneously typed lists
type HList = [Dynamic]

-- The empty list
initHList :: HList
initHList = []

-- Add an entry
addHList :: Typeable a => a -> HList -> HList
addHList a l = (toDyn a:l)

-- Test for an empty list
nullHList :: HList -> Bool
nullHList = null

-- Retrieve head by type case
headHList :: Typeable a => HList -> Maybe a
headHList [] = Nothing
headHList (a:_) = fromDynamic a

-- Retrieve head by type case
tailHList :: HList -> HList
tailHList = tail

-- Access per index; starts at 1
nth1HList :: Typeable a => Int -> HList -> Maybe a
nth1HList i l = fromDynamic (l !! (i-1))



----------------------------------------------------------------------------

-- A demo list
mylist = addHList (1::Int)       $
         addHList (True::Bool)   $
         addHList ("42"::String) $
         initHList


-- Main function for testing
main = print   ( show (nth1HList 1 mylist :: Maybe Int)    -- shows Just 1
             , ( show (nth1HList 1 mylist :: Maybe Bool)   -- shows Nothing
             , ( show (nth1HList 2 mylist :: Maybe Bool)   -- shows Just True
             , ( show (nth1HList 3 mylist :: Maybe String) -- shows Just "42"
             ))))
