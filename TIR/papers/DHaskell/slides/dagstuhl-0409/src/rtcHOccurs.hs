{-# OPTIONS -fglasgow-exts #-}

{-

This module illustrates weakly typed, heterogeneous lists.

-}

module Main where
import Prelude hiding (length)
import Data.Maybe
import Data.Typeable


-- Naturals
data Nat = Zero | Succ Nat deriving Eq


-- Length of lists
length :: [a] -> Nat
length [] = Zero
length (h:t) = Succ (length t)


-- Weakly typed heterogeneous lists
type HList    = [Castable]
data Castable = forall x. Typeable x => Castable x 
unCastable :: Typeable x => Castable -> Maybe x
unCastable (Castable x) = cast x


-- The empty list
initHList :: HList
initHList = []

-- Add an entry
addHList :: Typeable a => a -> HList -> HList
addHList a l = (Castable a:l)

-- Test for an empty list
nullHList :: HList -> Bool
nullHList = null

-- Retrieve head by type case
headHList :: Typeable a => HList -> Maybe a
headHList [] = Nothing
headHList (a:_) = unCastable a

-- Retrieve head by type case
tailHList :: HList -> HList
tailHList = tail

-- Access per index; starts at 1
nth1HList :: Typeable a => Int -> HList -> Maybe a
nth1HList i l = unCastable (l !! (i-1))

-- Look up all elements of a given type
hOccursMany :: Typeable a => HList -> [a]
hOccursMany [] = []
hOccursMany (h:t) = case unCastable h of
                      Just a  -> a : hOccursMany t
                      Nothing -> hOccursMany t


-- Insist on a single occurrence
rtcHOccurs :: Typeable a => HList -> Maybe a
rtcHOccurs hl = if length many == Succ Zero 
                  then Just (head many)
                  else Nothing
 where
  many = hOccursMany hl


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

angus = [ Castable (Key 42)
        , Castable (Name "Angus")
        , Castable Cow
        , Castable (Price 75.5) 
        ] 


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
