{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

data HTrue; hTrue :: HTrue; hTrue = undefined
data HFalse; hFalse :: HFalse; hFalse = undefined
class HBool x; instance HBool HTrue; instance HBool HFalse
instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"

typeEq :: TypeEq t t' b => t -> t' -> b
typeEq = undefined

class TypeEq' () x y b => TypeEq x y b | x y -> b
class TypeEq' q x y b | q x y -> b
class TypeEq'' q x y b | q x y -> b
instance TypeEq' () x y b => TypeEq x y b
instance TypeEq' () x x HTrue
instance TypeEq'' q x y b => TypeEq' q x y b
instance TypeEq'' () x y HFalse

data HNil      = HNil      deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)

class HList l
instance HList HNil
instance HList l => HList (HCons e l)
 
data Nil a    = Nil
data Cons b a = Cons a (b a)

class List ( lst :: * -> * )
instance List Nil
instance List b => List (Cons b)

class Type2Value t v | t -> v
 where
  type2value :: t -> v

instance Type2Value (Nil a) [a]
 where
  type2value _ = []

instance Type2Value (b a) [a]
      => Type2Value (Cons b a) [a]
 where
  type2value (Cons h t) = h : type2value t

class HOccursMany il ol e | il e -> ol
 where
  hOccursMany :: il -> ol e

instance HOccursMany HNil Nil e
 where
  hOccursMany HNil = Nil

instance ( TypeEqBool h e b
         , HOccursManyBool b (HCons h t) l e
         )
      =>   HOccursMany (HCons h t) (Cons b) e
 where
  hOccursMany (HCons h t) = result


Cons h (hOccursMany t)

{-
instance ( TypeEqBool 
HOccursMany l b e, HList l, List b )
      =>   HOccursMany (HCons e l) (Cons b) e
 where
  hOccursMany (HCons h t) = Cons h (hOccursMany t)

instance ( HOccursMany l b e, HList l, List b )
      =>   HOccursMany (HCons e l) b e
 where
  hOccursMany (HCons _ t) = hOccursMany t
-}

class  HOccurs e l
 where hOccurs :: l -> e

{-
instance HOccursMany l (Cons b) e
      => HOccurs e l
 where
  hOccurs l = let (Cons a _) = hOccursMany l in a
-}
