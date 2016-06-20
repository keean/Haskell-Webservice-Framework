{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

That's something what one might have expected to work.
There are two instances "TypeEq x x b" and "TypeEq x y b".
There is a functional dependency from x y -> b.
The two instances via an extra class enforce b to be
either TrueValue (first instance) or FalseValue (second instance).

Unfortuntaly:
- hugs still thinks that the instances are inconsistent with func deps.
- ghc 6.2 does not want to instantiate "b" to ???False:

typeEq4.hs:42:
    Cannot unify the type-signature variable `b'
        with the type `TrueValue'
        Expected type: TrueValue
        Inferred type: b
    When using functional dependencies to combine
      IsTrueValue () TrueValue,
        arising from the instance declaration at typeEq4.hs:26
      IsTrueValue () b,
        arising from the instance declaration at typeEq4.hs:42
    When trying to generalise the type inferred for `typeEq'
        Signature type:     forall x b. (IsTrueValue () b) => x -> x -> b
        Type to generalise: x -> x -> b

-}

-- The two TRUTH values
data TrueValue  = TrueValue  deriving Show
data FalseValue = FalseValue deriving Show

-- A class for two-valued logic
class TVL x
instance TVL TrueValue
instance TVL FalseValue

-- A class for testing for TrueValue
class IsTrueValue x y | x -> y
 where
  trueValue :: x -> y
instance IsTrueValue () TrueValue
 where
  trueValue _ = TrueValue

-- A class for testing for FalseValue
class IsFalseValue x y | x -> y
 where
  falseValue :: x -> y
instance IsFalseValue () FalseValue
 where
  falseValue _ = FalseValue

-- A class for a Boolean function for type equality
class TVL b => TypeEq x y b where
 typeEq :: x -> y -> b

instance IsTrueValue () b => TypeEq x x b
 where
  typeEq _ _ = trueValue ()

instance IsFalseValue () b => TypeEq x y b
 where
  typeEq _ _ = falseValue ()
