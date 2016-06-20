{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

append :: [a] -> [a] -> [a]
append [] l = l
append (x:l) l' = x : append l l'

data HNil = HNil
data HCons h t = HCons h t

class HList l
instance HList HNil
instance HList t => HList (HCons h t)

class (HList l, HList l', HList l'')
   =>  HAppend l l' l'' | l l' -> l''
 where hAppend :: l -> l' -> l''
 
instance HList l => HAppend HNil l l
 where hAppend HNil l = l
 
instance (HList l, HAppend l l' l'')
      => HAppend (HCons x l) l' (HCons x l'')
 where   hAppend (HCons x l) l' =
                  HCons x (hAppend l l')

