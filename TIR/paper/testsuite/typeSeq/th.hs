{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}
  
import HNat
import HList

data Lambda v e = Lambda v e

data Fun x y = Fun x y

class Expr vs e t

instance ( Assume vs v x vs'
         , HNat v
         , Expr vs' e y 
         )
           => Expr vs (Lambda v e) (Fun x y)
