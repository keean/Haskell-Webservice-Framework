{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
 
module HMap where

data HTrue = HTrue deriving Show
data HFalse = HFalse deriving Show


-- class HTypeEqBool a b c | a b -> c
-- instance HTypeEqBool x x HTrue
-- instance {- assuming x /= y -} HTypeEqBool x y HFalse

--   Functional dependencies conflict between instance declarations:


class HTypeEqBool x y r | x y -> r
instance HTypeEqBool x x HTrue
--instance {- assuming x /= y -} HTypeEqBool x y HFalse

class HTypeEqBool'  x y r | x y -> r
class HTypeEqBool'' x y r | x y -> r

instance HTypeEqBool' [x] y r => HTypeEqBool x y r
instance HTypeEqBool'' x y r => HTypeEqBool' x y r
instance HTypeEqBool'' [x] y HFalse

{-
class TypeNotEq x y
instance TypeNotEq x y -- matches with x = y, too.
instance Fail () => TypeNotEq x x
class Fail a
-}

class TypeEq x y
instance HTypeEqBool x y HTrue => TypeEq x y
class TypeNotEq x y
instance HTypeEqBool x y HFalse => TypeNotEq x y


teq:: (HTypeEqBool x y r) => x -> y -> r
teq = undefined


class HCast r a b where
    hcast:: r -> a -> Maybe b
    
instance HCast HTrue  a a where hcast _ = Just
instance HCast HFalse a b where hcast _ = const Nothing

cast::(HTypeEqBool a b r, HCast r a b) => a -> Maybe b
cast a :: Maybe b = hcast (teq a (undefined::b)) a

-- cast 'a' :: Maybe Int
-- cast 'a' :: Maybe Char