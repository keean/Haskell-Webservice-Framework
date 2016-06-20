{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Paper code demonstrating Haskell 98 records for animals.

-}


newtype Key     = Key Integer
                deriving (Show,Eq,Ord)
newtype Name   = Name String
                deriving (Show,Eq)
data Breed     = Cow | Sheep
                deriving (Show,Eq)
newtype Price  = Price Float
                deriving (Show,Eq,Ord)
data Disease   = BSE | FM 
                deriving (Show,Eq)

-- Haskell's records

data UnpricedAnimal = UnpricedAnimal {
          key   :: Integer
        , name  :: String
        , breed :: Breed
        }
     deriving Show

myUnpricedAnimal = UnpricedAnimal {
          key   = 42
        , name  = "Angus"
        , breed = Cow
        }


-- Session transcript

{-

Main> breed myUnpricedAnimal
Cow

Main> myUnpricedAnimal { breed = Sheep }
UnpricedAnimal{key=42,name="Angus",breed=Sheep}

-}
