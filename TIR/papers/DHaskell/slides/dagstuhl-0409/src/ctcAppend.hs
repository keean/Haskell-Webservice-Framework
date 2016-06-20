{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

import Prelude hiding (Eq,Bool)

{-

"-fallow-undecidable-instances" is needed for non-type variables in
constraints just as in "Show (b a)" for showing the tail of a list.
Otherwise pretty decidable.

-}


-- Type-level lists

data Nil a    = Nil
data Cons b a = Cons a (b a)

class List ( lst :: * -> * )
instance List Nil
instance List b => List (Cons b)


-- Type-level append

class (List l1, List l2, List l3) => Append l1 l2 l3 | l1 l2 -> l3
 where
  append :: l1 a -> l2 a -> l3 a

instance List x => Append Nil x x
 where
  append Nil x = x


{-

-- The correct Cons instance

instance Append t x t'
      => Append (Cons t) x (Cons t')
 where
    append (Cons h t) x = Cons h (append t x)

-}


-- {-

-- The flawed Cons instance

instance Append t x t' => Append (Cons t) x t'
 where
    append (Cons h t) x = append t x

-- -}


-- Type-level naturals
data Zero
data Succ n
class Nat n
instance Nat Zero
instance Nat n => Nat (Succ n)


-- Addition of type-level naturals
class (Nat n1, Nat n2, Nat n3) => Add n1 n2 n3
instance Nat n => Add Zero n n
instance Add n n' n'' => Add (Succ n) n' (Succ n'')


-- Length of type-level lists
class (List lst, Nat n) => Length lst n | lst -> n
instance Length Nil Zero
instance Length t len => Length (Cons t) (Succ len)


-- Compile-time-checked applications of append
ctCheckedAppend :: ( Append lst1 lst2 lst3
                   , Length lst1 nat1 
                   , Length lst2 nat2
                   , Length lst3 nat3
                   , Add nat1 nat2 nat12
                   , Eq nat3 nat12 True
                   ) => lst1 a -> lst2 a -> lst3 a
ctCheckedAppend = append


-- Type-level Booleans

data True
data False
class Bool x
instance Bool True
instance Bool False


-- Type-level equality

class Bool b => Eq x y b
instance Eq Zero Zero True
instance Eq (Succ x) Zero False
instance Eq Zero (Succ x) False
instance Eq x y b => Eq (Succ x) (Succ y) b
