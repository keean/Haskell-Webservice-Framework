{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

{-

A verified append.
The definition of append itself is proven to meet the length property.

-}


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


-- Type-level lists
data Nil len a    = Nil          deriving Show
data Cons b len a = Cons a (b a) deriving Show
class List ( lst :: * -> * )
instance List (Nil Zero)
instance List (b len) => List (Cons (b len) (Succ len))


-- The verified append

class ( List (l1 len1), List (l2 len2), List (l3 len3)
      , Add len1 len2 len3
      )
   => Append l1 len1 l2 len2 l3 len3 | l1 len1 l2 len2 -> l3 len3
 where
  append :: l1 len1 a -> l2 len2 a -> l3 len3 a

instance (Nat len, List (x len)) => Append Nil Zero x len x len
 where
  append Nil x = x


-- {-

-- The correct Cons instance

instance Append t len x xlen t' len'
      => Append (Cons (t len)) (Succ len) x xlen (Cons (t' len')) (Succ len')
 where
  append (Cons h t) x = Cons h (append t x)

-- -}


{-

-- A flawed Cons instance

instance Append t len x xlen t' len'
      => Append (Cons (t len)) (Succ len) x xlen t' len'
 where
  append (Cons h t) x = append t x

{-

Type error disproves claim about lengths:

    Could not deduce (Add (Succ len) xlen len')
        from the context (Append t len x xlen t' len')
      arising from the instance declaration at append.hs:194
    Probable fix:
        Add (Add (Succ len) xlen len')
        to the instance declaration superclass context
        Or add an instance declaration for (Add (Succ len) xlen len')
    In the instance declaration for `Append (Cons (t len)) (Succ len) x xlen t' len''

-}

-}
