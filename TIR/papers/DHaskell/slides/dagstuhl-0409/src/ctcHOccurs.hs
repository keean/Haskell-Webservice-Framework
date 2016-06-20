{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

This module illustrates strongly typed, heterogeneous lists.

-}

module Main where
import Prelude hiding (Maybe,Just,Nothing,Eq,Bool,head)


-- Type-level naturals
data Zero
data Succ n
class Nat n
instance Nat Zero
instance Nat n => Nat (Succ n)


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


-- Length of type-level lists
class (List lst, Nat n) => Length lst n | lst -> n
instance Length Nil Zero
instance Length t len => Length (Cons t) (Succ len)


-- Type-level maybes

data Nothing x = Nothing
data Just x    = Just x

class Maybe (maybe :: * -> *)
instance Maybe Nothing
instance Maybe Just


-- Type-level cast

class (Maybe m, Cast' () x m y) => Cast x m y | x y -> m
 where
  cast :: x -> m y

instance Cast' () x m y => Cast x m y
 where
  cast x = cast' () x

class Maybe m => Cast' t x m y | t x y -> m
 where
  cast' :: t -> x -> m y

instance Cast' () x Just x 
 where
  cast' _ x = Just x

instance Cast'' t x m y => Cast' t x m y 
 where
  cast' t x = cast'' t x

class Maybe m => Cast'' t x m y | t x y -> m
 where
  cast'' :: t -> x -> m y

instance Cast'' () x Nothing y 
 where
  cast'' t x = Nothing


-- HLists

data HNil = HNil
data HCons h t = HCons h t

class HList l
instance HList HNil
instance HList t => HList (HCons h t)


-- Type-level lists

data Nil a    = Nil
data Cons b a = Cons a (b a)

class List ( lst :: * -> * )
 where
  list :: lst a -> [a]

instance List Nil
 where
  list Nil = []

instance List b => List (Cons b)
 where
  list (Cons h t) = h:list t


-- Look up all elements of a given type

class (HList hl, List lst)
   =>  HOccursMany a hl lst | hl a -> lst
 where
  hOccursMany :: hl -> lst a

instance HOccursMany a HNil Nil
 where
  hOccursMany HNil = Nil

instance (Cast h m a, HOccursManyCase m a t lst)
      =>  HOccursMany a (HCons h t) lst
 where
  hOccursMany (HCons h t) = hOccursManyCase (cast h) t

class (Maybe m, HList hl, List lst)
   =>  HOccursManyCase m a hl lst | m hl a -> lst
 where
  hOccursManyCase :: m a -> hl -> lst a

instance (HList hl, List lst, HOccursMany a hl lst)
      =>  HOccursManyCase Just a hl (Cons lst)
 where 
  hOccursManyCase (Just a) hl = Cons a (hOccursMany hl) 

instance (HList hl, List lst, HOccursMany a hl lst)
      =>  HOccursManyCase Nothing a hl lst
 where
  hOccursManyCase Nothing hl = hOccursMany hl


ctcHOccurs :: ( HOccursMany a hl lst
              , Length lst len
              , Eq len (Succ Zero) True
              , Head lst
              )
           => hl -> a
ctcHOccurs hl = head many
 where
  many = hOccursMany hl


class Head (lst :: * -> *)
 where
  head :: lst a -> a

instance List l => Head (Cons l)
 where
  head (Cons h _) = h
