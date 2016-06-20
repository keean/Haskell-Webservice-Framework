import Prelude hiding (length)
 
-- List append
append :: [a] -> [a] -> [a]
append [] x = x
append (h:t) x = h:(append t x)
 
-- Naturals
data Nat = Zero | Succ Nat deriving Eq
 
-- Addition of naturals
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) n' = Succ (add n n')

-- Length of lists
length :: [a] -> Nat
length [] = Zero
length (h:t) = Succ (length t)

-- Run-time-checked append 
rtcAppend :: [a] -> [a] -> Maybe [a]
rtcAppend lst1 lst2
   = if length lst3 == add (length lst1) (length lst2)
        then Just lst3
        else Nothing
 where
  lst3 = append lst1 lst2
