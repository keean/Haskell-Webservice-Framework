{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-based services.

-}


module HType where

import Data.Typeable
import FakePrelude


{-----------------------------------------------------------------------------}

-- A phantom type for type proxies

data HProxy e = HProxy deriving (Show,Typeable)

hProxy   :: e -> HProxy e
hProxy _ =  HProxy


{-----------------------------------------------------------------------------}

-- Type equivalence is easy

class TypeEq x y
instance TypeEq x x


{-----------------------------------------------------------------------------}

-- Type non-equivalence seemed difficult, but ...

class TypeNotEq x y
 -- the method is for testing only
 where
  hTypeNotEq :: HProxy x -> HProxy y -> Bool
  hTypeNotEq _ _ = True


instance Fail () => TypeNotEq x x
instance TypeNotEq x y

-- A class without instances
class Fail z


{-----------------------------------------------------------------------------}

-- Type-safe cast is trivial too

class TypeSafeCast a a'
 where
  typeSafeCast :: a -> Maybe a'
 
instance TypeSafeCast a a
 where
  typeSafeCast = Just
 
instance TypeSafeCast a a'
 where
  typeSafeCast = const Nothing



{-----------------------------------------------------------------------------}

class Force   a b   | a -> b   where force   :: a -> b
class Force'  t a b | t a -> b where force'  :: t->a->b
class Force'' t a b | t a -> b where force'' :: t->a->b
instance Force'  () a b => Force a b where force x = force' () x
instance Force'' t a b => Force' t a b where force' = force''
instance Force'' () a a where force'' _ x  = x

{-

Another encoding

class Force   a b | a -> b   where force :: a -> b
class Apply f x y | f x -> y where apply :: f -> x -> y
instance Apply () a b   => Force a b where force x = apply () x
instance Apply [()] x x where apply _ x = x
instance Apply [f] x y => Apply f x y where apply f x = apply [f] x

-}



{-----------------------------------------------------------------------------}

-- A predicate for type-safe equality

{-

class TypeEqBool x y b | x y -> b
 where
  typeEqBool :: HProxy x -> HProxy y -> b

instance TypeEqBool x x HTrue
 where
  typeEqBool _ _ = HTrue

instance Force HFalse b => TypeEqBool x y b
 where
  typeEqBool _ _ = force HFalse

-}

-- {-

-- A version that does not refer to Force

class TypeEqBool' () x y b => TypeEqBool x y b | x y -> b
 where
  typeEqBool :: HProxy x -> HProxy y -> b

instance TypeEqBool' () x y b => TypeEqBool x y b
 where
  typeEqBool x y = typeEqBool' () x y

class TypeEqBool' q x y b | q x y -> b
 where
  typeEqBool' :: q -> HProxy x -> HProxy y -> b

instance TypeEqBool' () x x HTrue
 where
  typeEqBool' () _ _ = HTrue

instance TypeEqBool'' q x y b => TypeEqBool' q x y b
 where
  typeEqBool' = typeEqBool''

class TypeEqBool'' q x y b | q x y -> b
 where
  typeEqBool'' :: q -> HProxy x -> HProxy y -> b

instance TypeEqBool'' () x y HFalse
 where
  typeEqBool'' () _ _ = HFalse

-- -}

data Ex = forall x. Ex x
data Un = Un (forall x. x)

class PTypeEq x y | x -> y
 where
  pTypeEq :: x -> y -> ()

instance PTypeEq x x
 where
  pTypeEq _ _ = ()


{-----------------------------------------------------------------------------}
