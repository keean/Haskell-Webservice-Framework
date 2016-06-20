{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Lib.TIR.HRecordGHC where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved.
------------------------------------------------------------------------------

import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic
-- import qualified Lib.TIR.Peano as Peano
import qualified Lib.TIR.TypeEq as Logic
import Lib.TIR.HList
-- import Lib.TIR.HArray
import Lib.TIR.HType
import Lib.TIR.HTypeGHC
import Lib.TIR.HRecord

------------------------------------------------------------------------------

class (Record r,HList k) => RProject r k r' | r k -> r' where
	rProject :: r -> k -> r'
instance HList k => RProject HNil k HNil where
	rProject _ _ = HNil
instance (HContainsType k x t,RProject' t (x,y) r k r') => RProject (HCons (x,y) r) k r' where
	rProject (HCons (x,y) r) k = rProject' (hContainsType k x) (x,y) r k
class (Record r,HList k) => RProject' t p r k r' | t p r k -> r' where
	rProject' :: t -> p -> r -> k -> r'
instance RProject r k r' => RProject' Logic.AllTrue p r k (HCons p r') where
	rProject' _ p r k = HCons p (rProject r k)
instance RProject r k r' => RProject' Logic.AllFalse p r k r' where
	rProject' _ _ r k = rProject r k

class Record r => RContainsKey r a t | r a -> t where
	rContainsKey :: r -> a -> t
instance RContainsKey HNil a Logic.AllFalse where
	rContainsKey _ _ = Logic.AllFalse
instance (Logic.TypeEq a k l,RContainsKey' l r a t) => RContainsKey (HCons (k,v) r) a t where
	rContainsKey _ a = rContainsKey' (Logic.typeEq a (undefined::k) :: l) (undefined::r) a
class (Record r,Logic.MBool l) => RContainsKey' l r a t | l r a -> t where
	rContainsKey' :: l -> r -> a -> t
instance Record r => RContainsKey' Logic.AllTrue r a Logic.AllTrue where
	rContainsKey' _ _ _ = Logic.AllTrue
instance RContainsKey r a t => RContainsKey' Logic.AllFalse r a t where
	rContainsKey' _ r a = rContainsKey r a
