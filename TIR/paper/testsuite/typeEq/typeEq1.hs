{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

Summary:
- hugs refuses becauses of inconsistency with func. deps.
- ghci works fine, only it finally admits inconsistency with func. deps.

So then are we allowed to use such a program?
Ralf says: NO!

$ hugs -98 +O typeEq0.hs
ERROR "typeEq0.hs" - Instances are not consistent with dependencies

$ ghci typeEq0.hs

ghci> typeEq "1" "2"
TrueValue

gchi> typeEq "1" "2" :: FalseValue
Couldn't match `TrueValue' against `FalseValue'
    Expected type: TrueValue
    Inferred type: FalseValue
When using functional dependencies to combine
  TypeEq' (W x) x TrueValue,
    arising from the instance declaration at typeEq0.hs:34
  TypeEq' (W [Char]) [Char] FalseValue,
    arising from use of `typeEq' at <interactive>:1
When generalising the type(s) for `it'

> typeEq "1" (2::Int)
FalseValue
> typeEq "1" (2::Int) :: TrueValue
 
Couldn't match `FalseValue' against `TrueValue'
    Expected type: FalseValue
    Inferred type: TrueValue
When using functional dependencies to combine
  TypeEq'' (W x) y FalseValue,
    arising from the instance declaration at typeEq0.hs:59
  TypeEq'' (W [Char]) Int TrueValue,
    arising from use of `typeEq' at <interactive>:1
When generalising the type(s) for `it'

-}


data TrueValue  = TrueValue  deriving Show
data FalseValue = FalseValue deriving Show

class TVL x
instance TVL TrueValue
instance TVL FalseValue

class TVL t => TypeEq x y t where
 typeEq :: x -> y -> t

newtype W a = W a

instance TypeEq' (W x) y t => TypeEq x y t where
 typeEq x y = typeEq' (W x) y

class TVL t => TypeEq' x y t | x y -> t
 where
  typeEq' :: x -> y -> t

instance TypeEq' (W x) x TrueValue
 where
  typeEq' _ _ = TrueValue

instance TypeEq'' x y t => TypeEq' x y t
 where
  typeEq' x y = typeEq'' x y

class TVL t => TypeEq'' x y t | x y -> t
 where
  typeEq'' :: x -> y -> t

instance TypeEq'' (W x) y FalseValue
 where
  typeEq'' _ _ = FalseValue 
