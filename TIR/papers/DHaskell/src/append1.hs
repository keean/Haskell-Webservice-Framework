{-

Normal textbook-style append for lists.

-}

import Prelude hiding (length)


-- Lists
data List a = Nil | Cons a (List a)


-- List append
append :: List a -> List a -> List a
append Nil x = x
append (Cons h t) x = Cons h (append t x)


-- Naturals
data Nat = Zero | Succ Nat deriving Eq


-- Addition of naturals
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) n' = Succ (add n n')


-- Length of lists
length :: List a -> Nat
length Nil = Zero
length (Cons h t) = Succ (length t)


-- We could enforce the add constraint at run-time.
guardedAppend :: List a -> List a -> Maybe (List a)
guardedAppend lst1 lst2
   = if length lst3 == add (length lst1) (length lst2) 
        then Just lst3
        else Nothing
 where
  lst3 = append lst1 lst2
