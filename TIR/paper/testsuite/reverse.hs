{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}



data            HNil      = HNil      deriving (Eq,Show,Read)
data HList l => HCons e l = HCons e l deriving (Eq,Show,Read)

class HList l
instance HList HNil
instance HList l => HList (HCons e l)

class (HList l,HList l',HList l'') => HReverse l l' l'' | l l' -> l''
 where
  hReverse :: l -> l' -> l''

instance HList l => HReverse HNil l l
 where
  hReverse _ l =  l

instance (HList l',HReverse l (HCons a l') l'') 
      => HReverse (HCons a l) l' l''
 where
  hReverse (HCons a l) l' = hReverse l (HCons a l')


class (HList l,HList l',HList l'')
   => HDeleteFst l l' a l'' | l l' a -> l''
 where
   hDeleteFst :: l -> l' -> a -> l''

instance HReverse l' l l'' => HDeleteFst (HCons a l) l' a l''
 where
  hDeleteFst (HCons _ l) l' _ = hReverse l' l

instance (HList l',HDeleteFst l (HCons b l') a l'')
      => HDeleteFst (HCons b l) l' a l''
 where
  hDeleteFst (HCons b l) l' a = hDeleteFst l (HCons b l') a

