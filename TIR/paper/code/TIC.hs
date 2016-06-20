{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed co-products.

-}


module TIC where

import Data.Typeable
import Data.Dynamic
import HType
import FakePrelude
import HList
import HTypeDriven
import TIP


{-----------------------------------------------------------------------------}

-- A datatype for type-indexed co-products

data TIC l = TIC Dynamic


{-----------------------------------------------------------------------------}

-- Public constructor

mkTIC :: ( HTypeProxied l
         , HBoundType (HProxy i) l
         , Typeable i
         ) 
      => i -> TIC l

mkTIC i = TIC (toDyn i)


{-----------------------------------------------------------------------------}

-- Public destructor

unTIC :: ( HTypeProxied l
         , HBoundType (HProxy o) l
         , Typeable o
         ) 
      => TIC l -> Maybe o

unTIC (TIC i) = fromDynamic i


{-----------------------------------------------------------------------------}

-- A type-indexed type sequence that is a sequence of proxy types

class HTypeIndexed l => HTypeProxied l
instance HTypeProxied HNil
instance ( HTypeProxied l
         , HFreeType (HProxy e) l
         )
           => HTypeProxied (HCons (HProxy e) l)


{-----------------------------------------------------------------------------}

-- TICs are opaque

instance Show (TIC l)
 where
  show _ = "<Apply hOut to TICs!>"


{-----------------------------------------------------------------------------}

-- Convenience notation for HProxyIndexed type sequences

infixr 3 :+:
type e :+: l = HCons (HProxy e) l

infixr 3 .+.
(.+.) :: HTypeProxied (HCons (HProxy e) l)
      => e -> TIP l -> TIP (HCons (HProxy e) l)

e .+. r = hExtend (hProxy e) r


{-----------------------------------------------------------------------------}

-- Sample code

type AnimalCol = Key :+: Name :+: Breed :+: Price :+: HNil

myCol = mkTIC Cow :: TIC AnimalCol

{-

*TIC> unTIC myCol :: Maybe Breed
Just Cow
*TIC> unTIC myCol :: Maybe Price
Nothing
*TIC> mkTIC "42" :: TIC AnimalCol
Type error ...
*TIC> unTIC myCol :: Maybe String
Type error ...

-}
