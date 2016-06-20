{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- Stanamic naturals
 
data VNat    = VZero | VSucc VNat deriving Show
data TZero   = TZero              deriving Show
data TSucc n = TSucc n            deriving Show

class Nat n
instance Nat VNat
instance Nat TZero
instance Nat n => Nat (TSucc n)

class TNat n
 where
  onTNat :: n -> r -> (forall n'. TNat n' => n' -> r) -> r
  
instance TNat TZero
 where
  onTNat _ r _ = r

instance TNat n => TNat (TSucc n)
 where
  onTNat (TSucc n) _ f = f n

tn2vn :: TNat n => n -> VNat
tn2vn tn = onTNat tn VZero (\tn' -> VSucc (tn2vn tn'))

vn2tn :: VNat -> (forall n. TNat n => n -> w) -> w
vn2tn VZero f      = f TZero
vn2tn (VSucc vn) f = vn2tn vn (\tn -> f (TSucc tn))
