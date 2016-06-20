data BNil = BNil
data B1 n = B1 n
data B0 n = B0 n

instance Nat BNil
instance Nat n => Nat (B1 n)
instance Nat n => Nat (B0 n)

class (Nat n,Nat m) => BInc n m | n -> m where
   bInc :: n -> m
instance BInc BNil (B0 BNil) where
   bInc BNil = B0 BNil
instance Nat n => BInc (B0 n) (B1 n) where
   bInc (B0 ns) = B1 ns
instance BInc n m => BInc (B1 n) (B0 m) where
   bInc (B1 ns) = B0 $ bInc ns

class (Nat n,Nat m) => BDec n m | n -> m where
   bDec :: n -> m
instance BDec (B0 BNil) BNil where
   bDec (B0 BNil) = BNil
instance Nat n => BDec (B1 n) (B0 n) where
   bDec (B1 ns) = B0 ns
instance BDec n m => BDec (B0 (B1 n)) (B1 (B0 m)) where
   bDec (B0 (B1 ns)) = B1 $ B0 $ bDec ns
instance BDec n m => BDec (B0 (B0 n)) (B1 (B1 m)) where
   bDec (B0 (B0 ns)) = B1 $ B1 $ bDec ns

bZero :: BNil
bZero = BNil

bOne :: B0 BNil
bOne = bInc bZero

bTwo :: B1 BNil
bTwo = bInc bOne

bThree :: B0 (B0 BNil)
bThree = bInc bTwo

bFour :: B1 (B0 BNil)
bFour = bInc bThree

bFive :: B0 (B1 BNil)
bFive = bInc bFour

bSix :: B1 (B1 BNil)
bSix = bInc bFive

bSeven :: B0 (B0 (B0 BNil))
bSeven = bInc bSix
