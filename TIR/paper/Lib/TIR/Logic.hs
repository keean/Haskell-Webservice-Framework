{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.TIR.Logic where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, All Rights Reserved.
------------------------------------------------------------------------------
-- As this logic is based on types, and classes which are open ended
-- I have based the implementation on:
--
-- Supra-Logic: Using Transfinite Type Theory with Type Variables
--  for Paraconsistancy. [Jorgen Villadsen, WCP-2003]
-- 
-- This is a logic based on True and False types with an infinite set
-- of unknown types (numbered 1 -> infinity)

import qualified Prelude
import Lib.TIR.Control

------------------------------------------------------------------------------

data True = True
data False = False
data Unknown = Unknown

false :: False
false = False

true :: True
true = True

unknown :: Unknown
unknown = Unknown

------------------------------------------------------------------------------

newtype W a = W { w :: a }

class TVL t where
    toBool :: t -> Prelude.Bool
instance TVL True where
    toBool _ = Prelude.True
instance TVL False where
    toBool _ = Prelude.False
instance TVL Unknown where
    toBool _ = Prelude.undefined

instance Prelude.Show True where
    showsPrec _ _ = Prelude.showString "True"
instance Prelude.Show False where
    showsPrec _ _ = Prelude.showString "False"
instance Prelude.Show Unknown where
    showsPrec _ _ = Prelude.showString "Unknown"

------------------------------------------------------------------------------
-- This is also model of a Transmission gate

class TVL t => Conditional t x y z | t x y -> z where
    cond :: t -> x -> y -> z
instance Conditional False x y y where
    cond _ _ y = y
instance Conditional True x y x where
    cond _ x _ = x

------------------------------------------------------------------------------

data ConditionalTestFailed = ConditionalTestFailed

class Assert a where
    assert :: a -> ()
instance Assert True where
    assert _ = ()
instance AssertFail ConditionalTestFailed => Assert x where
    assert _ = ()

------------------------------------------------------------------------------

class TypeEq' (W x) y t => TypeEq x y t | x y -> t where
    typeEq :: x -> y -> t
instance TypeEq' (W x) y t => TypeEq x y t where
    typeEq x y = typeEq' (W x) y
class TVL t => TypeEq' x y t | x y -> t where
   typeEq' :: x -> y -> t
instance TypeEq' (W x) x True where
    typeEq' _ _ = true
instance TypeEq'' x y t => TypeEq' x y t where
    typeEq' x y = typeEq'' x y
class TVL t => TypeEq'' x y t | x y -> t where
    typeEq'' :: x -> y -> t
instance TypeEq'' (W x) y False where
    typeEq'' _ _ = false

class TypeNe' (W x) y t => TypeNe x y t | x y -> t where
    typeNe :: x -> y -> t
instance TypeNe' (W x) y t => TypeNe x y t where
    typeNe x y = typeNe' (W x) y
class TVL t => TypeNe' x y t | x y -> t where
   typeNe' :: x -> y -> t
instance TypeNe' (W x) x False where
    typeNe' _ _ = false
instance TypeNe'' x y t => TypeNe' x y t where
    typeNe' x y = typeNe'' x y
class TVL t => TypeNe'' x y t | x y -> t where
    typeNe'' :: x -> y -> t
instance TypeNe'' (W x) y True where
    typeNe'' _ _ = true
    
------------------------------------------------------------------------------
-- truth table for NOT: [T=True, F=False, 1 and 2 represent types neither true 
--  nor false, effectively an infinite set of unknowns]
-- ¬
-- T F
-- F T
-- 1 1
-- 2 2
-- 3 3 

infixl 2 `not`
class Not' (W x) t => Not x t | x -> t where
    not :: x -> t
instance Not' (W x) t => Not x t where
    not x = not' (W x)
class Not' x t | x -> t where
    not' :: x -> t
instance Not' (W False) True where
    not' _ = true
instance Not' (W True) False where
    not' _ = false
instance Not'' x t => Not' x t where
    not' x = not'' x
class Not'' x t | x -> t where
    not'' :: x -> t
instance Not'' (W x) x where
    not'' (W x) = x

------------------------------------------------------------------------------
-- truth table for AND:
--
-- ^ T F 1 2 3
-- T T F 1 2 3
-- F F F F F F
-- 1 1 F 1 F F
-- 2 2 F F 2 F
-- 3 3 F F F 3

infixl 4 `and`
class And' (W x) (W y) t => And x y t | x y -> t where
    and :: x -> y -> t
instance And' (W x) (W y) t => And x y t where
    and x y = and' (W x) (W y)
class And' x y t | x y -> t where
    and' :: x -> y -> t
instance And' (W x) (W x) x where
    and' (W x) _ = x
instance And'' x y t => And' x y t where
   and' x y = and'' x y
class And'' x y t | x y -> t where
   and'' :: x -> y -> t
instance And'' x (W False) False where
    and'' _ _ = false
instance And''' x y t => And'' x y t where
    and'' x y = and''' x y
class And''' x y t | x y -> t where
    and''' :: x -> y -> t
instance And''' (W False) x False where
    and''' _ _ = false
instance And'''' x y t => And''' x y t where
   and''' x y = and'''' x y
class And'''' x y t | x y -> t where
   and'''' :: x -> y -> t
instance AssertTypeNe x y => And'''' (W x) (W y) False where
    and'''' _ _ = false

------------------------------------------------------------------------------
-- truth table for OR:
--
-- v T F 1 2 3
-- T T T T T T
-- F T F 1 2 3
-- 1 T 1 1 T T
-- 2 T 2 T 2 T
-- 3 T 3 T T 3

infixl 6 `or`
class Or' (W x) (W y) t => Or x y t | x y -> t where
    or :: x -> y -> t
instance Or' (W x) (W y) t => Or x y t where
    or x y = or' (W x) (W y)
class Or' x y t | x y -> t where
    or' :: x -> y -> t
instance Or' (W True) (W x) True where
    or' _ _ = true
instance Or' (W False) (W x) x where
    or' _ (W x) = x
instance Or'' x y t => Or' x y t where
    or' x y = or'' x y
class Or'' x y t | x y -> t where
    or'' :: x -> y -> t
instance Or'' (W x) (W True) True where
    or'' _ _ = true
instance Or'' (W x) (W False) x where
    or'' (W x) _ = x
instance Or''' x y t => Or'' x y t where
    or'' x y = or''' x y
class Or''' x y t | x y -> t where
    or''' :: x -> y -> t
instance Or''' (W x) (W x) x where
    or''' (W x) _ = x
instance Or'''' x y t => Or''' x y t where
    or''' x y = or'''' x y
class Or'''' x y t | x y -> t where
    or'''' :: x -> y -> t
instance AssertTypeNe x y => Or'''' (W x) (W y) True where
    or'''' _ _ = true

------------------------------------------------------------------------------
-- truth table for XOR:
--
--_v_T F 1 2 3
-- T F T 1 2 3
-- F T F 1 2 3
-- 1 1 1 1 F F
-- 2 2 2 F 2 F
-- 3 3 3 F F 3

infixl 8 `xor`
class Xor' (W x) (W y) t => Xor x y t | x y -> t where
    xor :: x -> y -> t
instance Xor' (W x) (W y) t => Xor x y t where
    xor x y = xor' (W x) (W y)
class Xor' x y t | x y -> t where
    xor' :: x -> y -> t
instance Xor' (W True) (W True) False where
   xor' _ _ = false
instance Xor' (W True) (W False) True where
    xor' _ _ = true
instance Xor' (W False) (W True) True where
    xor' _ _ = true
instance Xor' (W False) (W False) False where
   xor' _ _ = false
instance Xor'' x y t => Xor' x y t where
    xor' x y = xor'' x y
class Xor'' x y t | x y -> t where
    xor'' :: x -> y -> t
instance Xor'' (W True) (W x) x where
    xor'' _ (W x) = x
instance Xor'' (W False) (W x) x where
    xor'' _ (W x) = x
instance Xor''' x y t => Xor'' x y t where
    xor'' x y = xor''' x y
class Xor''' x y t | x y -> t where
    xor''' :: x -> y -> t
instance Xor''' (W x) (W True) x where
    xor''' (W x) _ = x
instance Xor''' (W x) (W False) x where
    xor''' (W x) _ = x
instance Xor'''' x y t => Xor''' x y t where
    xor''' x y = xor'''' x y
class Xor'''' x y t | x y -> t where
    xor'''' :: x -> y -> t
instance Xor'''' (W x) (W x) x where
    xor'''' (W x) _ = x
instance Xor''''' x y t => Xor'''' x y t where
   xor'''' x y = xor''''' x y
class Xor''''' x y t | x y -> t where
   xor''''' :: x -> y -> t
instance AssertTypeNe x y => Xor''''' (W x) (W y) False where
    xor''''' _ _ = false

------------------------------------------------------------------------------
-- truth table for IMP:
--
-- -> T F 1 2 3
--  T T F 1 2 3
--  F T T T T T
--  1 T 1 T 1 1
--  2 T 2 2 T 2
--  3 T 3 3 3 T

class Imp' (W x) (W y) t => Imp x y t | x y -> t where
    imp :: x -> y -> t
instance Imp' (W x) (W y) t => Imp x y t where
    imp x y = imp' (W x) (W y)
class Imp' x y t | x y -> t where
    imp' :: x -> y -> t
instance Imp' (W False) (W x) True where
    imp' _ _ = true
instance Imp' (W True) (W x) x where
    imp' _ (W x) = x
instance Imp'' x y t => Imp' x y t where
    imp' x y =imp'' x y
class Imp'' x y t | x y -> t where
    imp'' :: x -> y -> t
instance Imp'' (W x) (W True) True where
    imp'' _ _ = true
instance Imp''' x y t => Imp'' x y t where
    imp'' x y =imp''' x y
class Imp''' x y t | x y -> t where
    imp''' :: x -> y -> t
instance Imp''' (W x) (W x) True where
    imp''' _ _ = true
instance Imp'''' x y t => Imp''' x y t where
    imp''' x y =imp'''' x y
class Imp'''' x y t | x y -> t where
    imp'''' :: x -> y -> t
instance AssertTypeNe x y => Imp'''' (W x) (W y) x where
    imp'''' (W x) _ = x

------------------------------------------------------------------------------
-- truth table for Bi-IMP:
--
-- <->T F 1 2 3
--  T T F 1 2 3
--  F F T 1 2 3
--  1 1 1 T F F
--  2 2 2 F T F
--  3 3 3 F F T

class IFF' (W x) (W y) t => IFF x y t | x y -> t where
    iff :: x -> y -> t
instance IFF' (W x) (W y) t => IFF x y t where
    iff x y = iff' (W x) (W y)
class IFF'  x y t | x y -> t where
    iff' :: x -> y -> t
instance IFF' (W False) (W True) False where
    iff' _ _ = false
instance IFF' (W True) (W False) False where
    iff' _ _ = false
instance IFF' (W x) (W x) True where
    iff' _ _ = true
instance IFF'' x y t => IFF' x y t where
    iff' x y = iff'' x y
class IFF'' x y t | x y -> t where
    iff'' :: x -> y -> t
instance IFF'' (W True) (W x) x where
    iff'' _ (W x) = x
instance IFF'' (W False) (W x) x where
    iff'' _ (W x) = x
instance IFF''' x y t => IFF'' x y t where
    iff'' x y = iff''' x y
class IFF''' x y t | x y -> t where
    iff''' :: x -> y -> t
instance IFF''' (W x) (W True) x where
    iff''' (W x) _ = x
instance IFF''' (W x) (W False) x where
    iff''' (W x) _ = x
instance IFF'''' x y t => IFF''' x y t where
    iff''' x y = iff'''' x y
class IFF'''' x y t | x y -> t where
    iff'''' :: x -> y -> t
instance AssertTypeNe x y => IFF'''' (W x) (W y) False where
    iff'''' _ _ = false

------------------------------------------------------------------------------
-- []
-- T T
-- F F
-- 1 F
-- 2 F
-- 3 F 

class Necessarily' (W x) t => Necessarily x t | x -> t where
    necessarily :: x -> t
instance Necessarily' (W x) t => Necessarily x t where
    necessarily x = necessarily' (W x)
class Necessarily' x t | x -> t where
    necessarily' :: x -> t
instance Necessarily' (W True) True where
    necessarily' _ = true
instance Necessarily'' x t => Necessarily' x t where
    necessarily' x = necessarily'' x
class Necessarily'' x t | x -> t where
    necessarily'' :: x -> t
instance Necessarily'' (W x) False where
    necessarily'' _ = false
    
------------------------------------------------------------------------------

-- <>
-- T T
-- F F
-- 1 T
-- 2 T
-- 3 T

class Possibly' (W x) t => Possibly x t | x -> t where
    possibly :: x -> t
instance Possibly' (W x) t => Possibly x t where
    possibly x = possibly' (W x)
class Possibly' x t | x -> t where
    possibly' :: x -> t
instance Possibly' (W True) True where
    possibly' _ = true
instance Possibly'' x t => Possibly' x t where
    possibly' x = possibly'' x
class Possibly'' x t | x -> t where
    possibly'' :: x -> t
instance Possibly'' (W x) False where
    possibly'' _ = false

------------------------------------------------------------------------------

class Determined x t | x -> t where
    determined :: x -> t
instance (Not x x',Or x x' xx,Necessarily xx t) => Determined x t where
    determined x = necessarily (x `or` (not x))

class Indetermined x t | x -> t where
   indetermined :: x -> t
instance (Not x x',Or x x' xx,Necessarily xx t',Not t' t) => Indetermined x t where
   indetermined x = not (determined x)

