{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Lib.DBC.Relation where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, All Rights Reserved.
------------------------------------------------------------------------------

data Zero = Zero deriving Show
data Suc n = Suc n deriving Show

class Nat n
instance Nat Zero
instance Nat n => Nat (Suc n)

zero :: Zero
zero = Zero

one :: Suc Zero
one = Suc zero

two :: Suc (Suc Zero)
two = Suc one

three :: Suc (Suc (Suc Zero))
three = Suc two

four :: Suc (Suc (Suc (Suc Zero)))
four = Suc three

five :: Suc (Suc (Suc (Suc (Suc Zero))))
five = Suc four

------------------------------------------------------------------------------

infixr 1 `RCons`
data RNil = RNil deriving Show
data RCons a r = a `RCons` r deriving Show

------------------------------------------------------------------------------

class Relation r where
   rHead :: a `RCons` r -> a
   rTail :: a `RCons` r -> r
   rIsEmpty :: r -> Bool
instance Relation RNil where
   rHead (x `RCons` _) = x
   rTail (_ `RCons` _) = RNil
   rIsEmpty RNil = True
instance Relation r => Relation (a `RCons` r) where
   rHead (x `RCons` _) = x
   rTail (_ `RCons` xs) = xs
   rIsEmpty (_ `RCons` _) = False

class RLast r a | r -> a where
   rLast :: r -> a
instance RLast (a `RCons` RNil) a where
   rLast (x `RCons` RNil) = x
instance RLast r b => RLast (a `RCons` r) b where
   rLast (_ `RCons` xs) = rLast xs

class RInit r1 r2 | r1 -> r2 where
   rInit :: r1 -> r2
instance RInit (a `RCons` RNil) RNil where
   rInit (_ `RCons` RNil) = RNil
instance RInit (b `RCons` r1) r2 => RInit (a `RCons` b `RCons` r1) (a `RCons` r2) where
   rInit (x `RCons` xs) = x `RCons` rInit xs

class REnqueue r1 r2 a | r1 a -> r2 where
   rEnqueue :: r1 -> a -> r2
instance REnqueue RNil (a `RCons` RNil) a where
   rEnqueue RNil y = y `RCons` RNil
instance REnqueue r1 r2 b => REnqueue (a `RCons` r1) (a `RCons` r2) b where
   rEnqueue (x `RCons` xs) y = x `RCons` rEnqueue xs y

class (Nat n,Relation r) => RIndex n r a | n r -> a where
   rIndex :: n -> r -> a
instance Relation r => RIndex Zero (a `RCons` r) a where
   rIndex Zero (x `RCons` _) = x
instance RIndex n r b => RIndex (Suc n) (a `RCons` r) b where
   rIndex (Suc n) (_ `RCons` xs) = rIndex n xs

infixl 2 `rProduct`
class (Relation r1,Relation r2,Relation r3) => RProduct r1 r2 r3 | r1 r2 -> r3 where
   rProduct :: r1 -> r2 -> r3
instance RProduct RNil RNil RNil where
   rProduct RNil RNil = RNil
instance Relation r => RProduct RNil r r where
   rProduct RNil r = r
instance RProduct r1 r2 r3 => RProduct (a `RCons` r1) r2 (a `RCons` r3) where
   rProduct (x `RCons` xs) y = x `RCons` (xs `rProduct` y)

------------------------------------------------------------------------------

class Relation r => RTuple t r | t -> r , r -> t where
   fromTuple :: t -> r
   toTuple :: r -> t

instance RTuple (a,b) (a `RCons` b `RCons` RNil) where
   fromTuple (a,b) = a `RCons` b `RCons` RNil
   toTuple (a `RCons` b `RCons` RNil) = (a,b)

instance RTuple (a,b,c) (a `RCons` b `RCons` c `RCons` RNil) where
   fromTuple (a,b,c) = a `RCons` b `RCons` c `RCons` RNil
   toTuple (a `RCons` b `RCons` c `RCons` RNil) = (a,b,c)

instance RTuple (a,b,c,d) (a `RCons` b `RCons` c `RCons` d `RCons` RNil) where
   fromTuple (a,b,c,d) = a `RCons` b `RCons` c `RCons` d `RCons` RNil
   toTuple (a `RCons` b `RCons` c `RCons` d `RCons` RNil) = (a,b,c,d)

instance RTuple (a,b,c,d,e) (a `RCons` b `RCons` c `RCons` d `RCons` e `RCons` RNil) where
   fromTuple (a,b,c,d,e) = a `RCons` b `RCons` c `RCons` d `RCons` e `RCons` RNil
   toTuple (a `RCons` b `RCons` c `RCons` d `RCons` e `RCons` RNil) = (a,b,c,d,e)

instance RTuple (a,b,c,d,e,f) (a `RCons` b `RCons` c `RCons` d `RCons` e `RCons` f `RCons` RNil) where
   fromTuple (a,b,c,d,e,f) = a `RCons` b `RCons` c `RCons` d `RCons` e `RCons` f `RCons` RNil
   toTuple (a `RCons` b `RCons` c `RCons` d `RCons` e `RCons` f `RCons` RNil) = (a,b,c,d,e,f)

------------------------------------------------------------------------------

{-

putStrLn $ show (rIndex two rel1) -- show the third item in rel1
putStrLn $ show (rHead r)
putStrLn $ show (rTail r)
putStrLn $ show (rLast r)
putStrLn $ show (rInit r)
putStrLn $ show (r `rEnqueue` "TEST3") -- insert the string into the last (not head) position
putStrLn $ show ((3 :: Int) `RCons` r) -- insert the Int into the head of the list
r = toTuple (( 1.1 :: Double) `RCons` (fromTuple ("hello",1,"World")))

-}

First redefine the RFold function to use RFoldFn class as its operator. Then create
instances of RFoldFn to do what you like. The clever bit is the use of an abstract
data-type to select which instance to use.



class RFold t i r where
  rFold :: t -> i -> r -> i
instance RFold t i RNil where
  rFold _ i RNil = i
instance (RFoldFn t a i,RFold t i r) => RFold t i (a `RCons` r) where
  rFold t i (x `RCons` xs) = rFoldFn t x (rFold t i xs)

class RFoldFn t a i where
  rFoldFn :: t -> a -> i -> i



Here's some examples:


data ShowFn = ShowFn
instance Show a => RFoldFn ShowFn a String where
  rFoldFn ShowFn x y = shows x y

putStrLn $ show $ rFold ShowFn "" r


data SumFn = SumFn
instance Num i => RFoldFn SumFn a i where
  rFoldFn SumFn _ s = 1 + s

putStrLn $ show $ rFold SumFn 0 r
