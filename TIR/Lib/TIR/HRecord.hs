{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Lib.TIR.HRecord where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved.
------------------------------------------------------------------------------

import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic
import qualified Lib.TIR.Peano as Peano
import qualified Lib.TIR.TypeEq as Logic
import Lib.TIR.HList
import Lib.TIR.HArray
import Lib.TIR.HType

------------------------------------------------------------------------------

type a :=: b = (a,b)
infixl 3 :=:

infixl 2 .=.
(.=.) :: a -> b -> (a,b)
a .=. b = (a,b)

infixl 9 !
(!) :: RLookup r k v => r -> k -> v
r ! k = rLookup r k

type HSelfLabelled a = (HProxy a,a)

hSelfLabelled :: a -> HSelfLabelled a
hSelfLabelled a = (HProxy,a)

------------------------------------------------------------------------------

class HTypeMap r
instance HTypeMap HNil
instance (HTypeMap r,HKeyFree k r) => HTypeMap (HCons (k,v) r)

data KeyNotFree k = KeyNotFree k
class HKeyFree k r
instance HKeyFree k HNil
instance Ctrl.AssertFail (KeyNotFree k) => HKeyFree k (HCons (k,v) r)
instance HKeyFree k r => HKeyFree k (HCons (k',v) r)

---------------------------------------------------------------------------

data HRecord r = HRecord { record :: r }

class Record r
instance Record HNil
instance Record r => Record (HCons (k,v) r)

data KeyNotFound k = KeyNotFound k

class Record r => RLookup r k v | r k -> v where
	rLookup :: r -> k -> v
instance Ctrl.AssertFail (KeyNotFound k) => RLookup HNil k () where
	rLookup _ _ = ()
instance (Logic.TypeEq k k' t,RLookup' t v' r k v) => RLookup (HCons (k',v') r) k v where
	rLookup (HCons (k',v) r) k = rLookup' (Logic.typeEq k k') v r k 
class Record r => RLookup' t v' r k v | t v' r k -> v where
	rLookup' :: t -> v' -> r -> k -> v
instance Record r => RLookup' Logic.AllTrue v r k v where
	rLookup' _ v _ _ = v
instance RLookup r k v => RLookup' Logic.AllFalse v' r k v where
	rLookup' _ _ r k = rLookup r k

class Record r => RValues r r' | r -> r' where
	rValues :: r -> r'
instance RValues HNil HNil where
	rValues _ = HNil
instance RValues r r' => RValues (HCons (k,v) r) (HCons v r') where
	rValues (HCons (_,v) r) = HCons v (rValues r)

class Record r => RKeys r r' | r -> r' where
	rKeys :: r -> r'
instance RKeys HNil HNil where
	rKeys _ = HNil
instance RKeys r r' => RKeys (HCons (k,v) r) (HCons k r') where
	rKeys (HCons (k,_) r) = HCons k (rKeys r)

class (Record r,HList v) => RNewValues r v r' | r v -> r' where
	rNewValues :: r -> v -> r'
instance RNewValues HNil HNil HNil where
	rNewValues _ _ = HNil
instance RNewValues r v r' => RNewValues (HCons (k,w') r) (HCons w v) (HCons (k,w) r') where
	rNewValues (HCons (k,_) r) (HCons w v) = HCons (k,w) (rNewValues r v)

class (Record r,HList k) => RNewKeys r k r' | r k -> r' where
	rNewKeys :: r -> k -> r'
instance RNewKeys HNil HNil HNil where
	rNewKeys _ _ = HNil
instance RNewKeys r k r' => RNewKeys (HCons (k,v) r) (HCons w k) (HCons (w,v) r') where
	rNewKeys (HCons (_,v) r) (HCons w k) = HCons (w,v) (rNewKeys r k)
{-
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
-}
{-
class Record r => RContainsKey r a t | r a -> t where
	rContainsKey :: r -> a -> t
instance RContainsKey HNil a Logic.AllFalse where
	rContainsKey _ _ = Logic.AllFalse
instance (Logic.TypeEq a k l,RContainsKey' l r a t) => RContainsKey (HCons (k,v) r) a t where
	rContainsKey (_::HCons b r) a = rContainsKey' (Logic.typeEq a (undefined::b)) (undefined::r) a
class (Record r,Logic.MBool l) => RContainsKey' l r a t | l r a -> t where
	rContainsKey' :: l -> r -> a -> t
instance RContainsKey' Logic.AllTrue r a Logic.AllTrue where
	rContainsKey' _ _ _ = Logic.AllTrue
instance RContainsKey r a t => RContainsKey' Logic.AllFalse r a t where
	rContainsKey' _ r a = rContainsKey r a
-}

class (Record r,HList k) => RProjectOrder2 r k r' | r k -> r' where
	rProjectOrder2 :: r -> k -> r'
instance Record r => RProjectOrder2 r HNil HNil where
	rProjectOrder2 _ _ = HNil
instance (RLookup r a v,RProjectOrder2 r k r') => RProjectOrder2 r (HCons a k) (HCons (a,v) r') where
	rProjectOrder2 r (HCons a k) = HCons (a,rLookup r a) (rProjectOrder2 r k)

class (Record r,Record r') => RRProject r r' where
	rrProject :: r -> r' -> r'
instance Record r => RRProject r HNil where
	rrProject _ _ = HNil
instance (RLookup r k v,RRProject r r') => RRProject r (HCons (k,v) r') where
	rrProject r (HCons (k,_) r') = HCons (k,rLookup r k) (rrProject r r')

class Record r => RMapValues f r r' | f r -> r' where
	rMapValues :: f -> r -> r'
instance RMapValues f HNil HNil where
	rMapValues _ _ = HNil
instance (Ctrl.Apply f v v',RMapValues f r r') => RMapValues f (HCons (k,v) r) (HCons (k,v') r') where
	rMapValues f (HCons (k,v) r) = HCons (k,Ctrl.apply f v) (rMapValues f r)

