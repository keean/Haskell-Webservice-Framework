{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.TIR.TypeEq where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved. 
------------------------------------------------------------------------------
-- Modal logic for partial systems.

--import qualified Prelude
import Lib.TIR.Control
import Lib.TIR.Logic

------------------------------------------------------------------------------

newtype W a = W { w :: a }

class TypeEq' (W x) y t => TypeEq x y t | x y -> t where
    typeEq :: x -> y -> t
instance TypeEq' (W x) y t => TypeEq x y t where
    typeEq x y = typeEq' (W x) y
class TypeEq' x y t | x y -> t where
   typeEq' :: x -> y -> t
instance {-# OVERLAPS #-} TypeEq' (W x) x AllTrue where
    typeEq' _ _ = AllTrue
instance {-# OVERLAPS #-} TypeEq'' x y t => TypeEq' x y t where
    typeEq' x y = typeEq'' x y
class TypeEq'' x y t | x y -> t where
    typeEq'' :: x -> y -> t
instance AssertTypeNe x y => TypeEq'' (W x) y AllFalse where
    typeEq'' _ _ = AllFalse
{-
class TypeNe' (W x) y t => TypeNe x y t | x y -> t where
    typeNe :: x -> y -> t
instance TypeNe' (W x) y t => TypeNe x y t where
    typeNe x y = typeNe' (W x) y
class TypeNe' x y t | x y -> t where
   typeNe' :: x -> y -> t
instance TypeNe' (W x) x AllFalse where
    typeNe' _ _ = AllFalse
instance TypeNe'' x y t => TypeNe' x y t where
    typeNe' x y = typeNe'' x y
class TypeNe'' x y t | x y -> t where
    typeNe'' :: x -> y -> t
instance AssertTypeNe x y => TypeNe'' (W x) y AllTrue where
    typeNe'' _ _ = AllTrue
-}
