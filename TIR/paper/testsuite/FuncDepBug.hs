{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

class C1 a b | a -> b
instance C1 Bool Int
instance C2 a b => C1 a b
class C2 a b | a -> b
instance C2 Bool Bool
