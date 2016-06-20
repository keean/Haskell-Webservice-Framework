{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

import Prelude hiding (length)

------------------------------------------------------------------------------
-- Normal value-level append
------------------------------------------------------------------------------

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


------------------------------------------------------------------------------
-- Type-level lists
------------------------------------------------------------------------------

data Nil' a    = Nil'          deriving Show
data Cons' b a = Cons' a (b a) deriving Show

class List' ( lst :: * -> * )
instance List' Nil'
instance List' b => List' (Cons' b)


class (List' l1, List' l2, List' l3)
   => Append' l1 l2 l3 | l1 l2 -> l3
 where
  append' :: l1 a -> l2 a -> l3 a

instance List' x => Append' Nil' x x
 where
  append' Nil' x = x


-- A possible flaw is illustrated

instance Append' t x t'
--    => Append' (Cons' t) x t'
      => Append' (Cons' t) x (Cons' t')
 where
--  append' (Cons' h t) x = append' t x
    append' (Cons' h t) x = Cons' h (append' t x)


-- Type-level naturals
data Zero'
data Succ' n
class Nat' n
instance Nat' Zero'
instance Nat' n => Nat' (Succ' n)


-- Addition of naturals
class (Nat' n1, Nat' n2, Nat' n3) => Add' n1 n2 n3
instance Nat' n => Add' Zero' n n
instance Add' n n' n'' => Add' (Succ' n) n' (Succ' n'')


-- Length of lists
class (List' lst, Nat' n) => Length' lst n | lst -> n
instance Length' Nil' Zero'
instance Length' t len => Length' (Cons' t) (Succ' len)


-- We could enforce the add constraint at compile-time.

guardedAppend' :: ( Append' lst1 lst2 lst3
                  , Length' lst1 n1, Length' lst2 n2, Length' lst3 n3
                  , Add' n1 n2 n3
                  ) => lst1 a -> lst2 a -> lst3 a
guardedAppend' = append'



------------------------------------------------------------------------------
-- Type-level lists indexed by naturals
------------------------------------------------------------------------------

data Nil'' len a    = Nil''          deriving Show
data Cons'' b len a = Cons'' a (b a) deriving Show

class List'' ( lst :: * -> * )
instance List'' (Nil'' Zero')
instance List'' (b len) => List'' (Cons'' (b len) (Succ' len))


class (List'' l1, List'' l2, List'' l3)
   => Append'' l1 l2 l3 | l1 l2 -> l3
 where
  append'' :: l1 a -> l2 a -> l3 a

instance List'' x => Append'' (Nil'' Zero') x x
 where
  append'' Nil'' x = x

{-

-- The correct instance

instance Append'' (t len) x (t' len')
      => Append'' (Cons'' (t len) (Succ' len))
                  x 
                  (Cons'' (t' len') (Succ' len'))
 where
    append'' (Cons'' h t) x = Cons'' h (append'' t x)

-}

-- {-

-- A flawed instance

instance Append'' (t len) x (t' len')
      => Append'' (Cons'' (t len) (Succ' len))
                  x 
                  (t' len')
 where
    append'' (Cons'' h t) x = append'' t x

-- -}


addConstraint :: Add' len len' len'' => f len a -> f len' a -> f len'' a
addConstraint =  undefined

guardedAppend'' l1 l2 | False = addConstraint l1 l2
guardedAppend'' l1 l2         = append'' l1 l2



------------------------------------------------------------------------------
-- A verified append
------------------------------------------------------------------------------


class ( List'' (l1 len1), List'' (l2 len2), List'' (l3 len3)
      , Add' len1 len2 len3
      )
   => Append''' l1 len1 l2 len2 l3 len3 | l1 len1 l2 len2 -> l3 len3
 where
  append''' :: l1 len1 a -> l2 len2 a -> l3 len3 a

instance (Nat' len, List'' (x len)) => Append''' Nil'' Zero' x len x len
 where
  append''' Nil'' x = x


-- {-

-- The correct instance

instance Append''' t len x xlen t' len'
      => Append''' (Cons'' (t len)) (Succ' len)
                   x xlen
                   (Cons'' (t' len')) (Succ' len')
 where
    append''' (Cons'' h t) x = Cons'' h (append''' t x)

-- -}

{-

-- A flawed instance

instance Append''' t len x xlen t' len'
      => Append''' (Cons'' (t len)) (Succ' len)
                   x xlen
                   t' len'
 where
    append''' (Cons'' h t) x = append''' t x

{-

Type error disproves claim about lengths:

    Could not deduce (Add' (Succ' len) xlen len')
        from the context (Append''' t len x xlen t' len')
      arising from the instance declaration at append.hs:194
    Probable fix:
        Add (Add' (Succ' len) xlen len')
        to the instance declaration superclass context
        Or add an instance declaration for (Add' (Succ' len) xlen len')
    In the instance declaration for `Append''' (Cons'' (t len)) (Succ' len) x xlen t' len''

-}

-}

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
