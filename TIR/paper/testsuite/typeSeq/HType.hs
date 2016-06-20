{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module HType where

import Data.Typeable
import HBool


{-----------------------------------------------------------------------------}

-- A phantom type for type proxies

data HProxy e = HProxy deriving (Show,Typeable)
hProxy :: e -> HProxy e
hProxy _ = HProxy


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

class HTypeSafeCast a a'
 where
  hTypeSafeCast :: a -> Maybe a'
 
instance HTypeSafeCast a a
 where
  hTypeSafeCast = Just
 
instance HTypeSafeCast a a'
 where
  hTypeSafeCast = const Nothing


{-----------------------------------------------------------------------------}


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


{-----------------------------------------------------------------------------}
