{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

   A streamlined version of the HList library.
   Just enough for what's needed in the DHaskell paper.

-}
 
module HList where


-- Type-level Booleans

data HTrue; hTrue :: HTrue; hTrue = undefined
data HFalse; hFalse :: HFalse; hFalse = undefined
class HBool x; instance HBool HTrue; instance HBool HFalse
instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"


-- Heterogeneous type sequences
 
data HNil      = HNil      deriving (Eq,Show,Read)
data HCons e l = HCons e l deriving (Eq,Show,Read)


-- The set of all types of heterogeneous lists

class HList l
instance HList HNil
instance HList l => HList (HCons e l)


-- Type equality predicate

class HBool b => TypeEq x y b | x y -> b
instance TypeEq x x HTrue
instance (HBool b, TypeCast HFalse b) => TypeEq x y b


-- Type-safe cast

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x


