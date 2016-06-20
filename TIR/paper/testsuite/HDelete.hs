{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- Illustration of the two-class trick -}

module HDelete where


data HNil = HNil deriving Show
data HCons a b = HCons a b deriving Show

class HList l
instance HList HNil
instance HList l => HList (HCons a l)

class HDeleteFst e l l' | e l -> l' where
    hdel:: e -> l -> l'

instance HDeleteFst e (HCons e l) l where
    hdel _ (HCons _ l) = l

{-   
instance HDeleteFst e l l' =>
        HDeleteFst e (HCons e' l) (HCons e' l') where
    hdel e (HCons e' l) = HCons e' (hdel e l)
-}

instance HDeleteFst' e l l' => HDeleteFst e l l' where
    hdel = hdel'
 

class HDeleteFst' e l l' | e l -> l' where
    hdel':: e -> l -> l'

instance HDeleteFst e l l' =>
        HDeleteFst' e (HCons e' l) (HCons e' l') where
    hdel' e (HCons e' l) = HCons e' (hdel e l)

tl = (HCons 'a' 
     (HCons True 
     (HCons "cd"
     (HCons False
     HNil))))
     
test1 = hdel "str" tl
test2 = hdel (undefined::Bool) tl
