{-# OPTIONS -fglasgow-exts #-}

{-

This module illustrates heterogeneously typed lists.

-}

module Main where
import Data.Dynamic
import Data.Maybe

-- Heterogeneously typed lists
type HListD = [Dynamic]

-- The empty list
initHList :: HListD
initHList = []

-- Add an entry
addHList :: Typeable a => a -> HListD -> HListD
addHList a l = (toDyn a:l)

-- Test for an empty list
nullHList :: HListD -> Bool
nullHList = null

-- Retrieve head by type case
headHList :: Typeable a => HListD -> Maybe a
headHList [] = Nothing
headHList (a:_) = fromDynamic a

-- Retrieve head by type case
tailHList :: HListD -> HListD
tailHList = tail

-- Access per index; starts at 1
nth1HList :: Typeable a => Int -> HListD -> Maybe a
nth1HList i l = fromDynamic (l !! (i-1))

-- Filter a list to retrieve all elements of a given type
hOccursMany :: Typeable a => HListD -> [a]
hOccursMany = map fromJust     -- unwrap Just
            . filter isJust    -- remove Nothing
            . map fromDynamic  -- get maybies

hOccursMany' :: Typeable a => HListD -> [a]
hOccursMany' [] = []
hOccursMany' (h:t) = case fromDynamic h of
                      Just a  -> a : hOccursMany' t
                      Nothing -> hOccursMany' t


----------------------------------------------------------------------------


newtype Key     = Key Integer
                deriving (Show,Eq,Ord,Typeable)
newtype Name   = Name String
                deriving (Show,Eq,Typeable)
data Breed     = Cow | Sheep
                deriving (Show,Eq,Typeable)
newtype Price  = Price Float
                deriving (Show,Eq,Ord,Typeable)
data Disease   = BSE | FM
                deriving (Show,Eq,Typeable)

myAnimal = [ toDyn (Key 42)
           , toDyn (Name "Angus")
           , toDyn Cow
           , toDyn (Price 75.5) 
           ] 

----------------------------------------------------------------------------

class HList l

----------------------------------------------------------------------------

class HList l => HFoldr f v l r | f v l -> r
  where
   hFoldr :: f -> v -> l -> r

----------------------------------------------------------------------------

instance HList [Dynamic]

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
