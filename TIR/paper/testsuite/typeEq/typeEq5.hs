{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- This is my idea of what should work! 
   My argument is essentially one of simplification, 
	in that if one instance is more specific, then
	they don't really conflict. The type checker should be less
	concerned with how a value is arrived at -}

data TrueValue  = TrueValue  deriving Show
data FalseValue = FalseValue deriving Show

class TVL x
instance TVL TrueValue
instance TVL FalseValue

class TypeEq x y t where
 typeEq :: x -> y -> t

newtype W a = W a

instance TypeEq' (W x) y t => TypeEq x y t where
 typeEq x y = typeEq' (W x) y

class TypeEq' x y t | x y -> t
 where
  typeEq' :: x -> y -> t

instance TypeEq' (W x) x TrueValue
 where
  typeEq' _ _ = TrueValue

instance TypeEq' x y FalseValue
 where
  typeEq' _ _ = FalseValue
