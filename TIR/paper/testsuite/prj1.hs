{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
 
module HMap where

-- easier to write then to do cvs update...
data HNil = HNil deriving Show
data HCons a b = HCons a b deriving Show

class HList l
instance HList HNil
instance HList l => HList (HCons a l)

-- Lists of pairs

class (HList l, HList f, HList s) => 
      HListOfPairs l f s | l -> f, l -> s, f s -> l where 
  hzip:: f -> s -> l
  hunzip1:: l -> f
  hunzip2:: l -> s

instance HListOfPairs HNil HNil HNil where
    hzip _ _ = HNil
    hunzip1 _ = HNil
    hunzip2 _ = HNil

instance HListOfPairs l f s => 
         HListOfPairs (HCons (a,b) l) (HCons a f) (HCons b s) where
    hzip (HCons a f) (HCons b s) = HCons (a,b) (hzip f s)
    hunzip1 (HCons (a,_) l) = HCons a (hunzip1 l)
    hunzip2 (HCons (_,b) l) = HCons b (hunzip2 l)
	 
-- BTW, it is certain that if HListOfPairs l f s holds, then the lengths
-- of all three lists are the same.
l1 = HCons (Just$ Just$ Just ()) $ HCons (Just$ Just ()) $
     HCons (Just ()) HNil
     
l2 = HCons True $ HCons 'a' $ HCons "ab" HNil

test1 = let l = hzip l1 l2 in (l, hunzip1 l, hunzip2 l)

-- we can't do the following any more
-- class HListOfPairs m f s => HMap m

-- but we can do this
class HMap m
instance HListOfPairs m f s => HMap m

-- No HSet constraint. I'm too lazy, due to Haskell
class HMapPrj m a b m1 | m a -> b m1 where
    prj:: m -> a -> (b,m1)

instance HMapPrj m HNil HNil m where
    prj m _ = (HNil,m)
    
instance (HMapAdd m1' a b m, HMapPrj m1' f s m1) =>
         HMapPrj m (HCons a f) (HCons b s) m1 where
    prj m (HCons a f) = let (b,m1') = prj1 m a
                            (s,m1)  = prj m1' f
			in (HCons b s, m1)


class HMapAdd m a b m1 | m1 a -> b m where
    prj1:: m1 -> a -> (b,m)
    
instance HMapAdd HNil a b (HCons (a,b) HNil) where
    prj1 (HCons (a,b) _) _ = (b,HNil)
    
instance HMapAdd m a b (HCons (a,b) m) where
    prj1 (HCons (a,b) m) _ = (b,m)
    
instance HMapAdd' m a b m1 => HMapAdd m a b m1 where
    prj1 = prj1'
    
class HMapAdd' m a b m1 | m1 a -> b m where
    prj1':: m1 -> a -> (b,m)

instance HMapAdd m a b m1 => 
         HMapAdd' (HCons (x,y) m) a b (HCons (x,y) m1) where
    prj1' (HCons x m1) a = let (b,m') = prj1 m1 a in (b, HCons x m')
    
-- inj:: (HMap m) => m -> (a,b) -> HCons (a,b) m
-- inj m x = HCons x m


hmap1 = hzip l1 l2 

--hmap2 = hProjectAway hmap1 (HCons x1 HNil)
hmap2 = snd $ prj hmap1 (HCons (Just$ Just ()) HNil)
--hmap3 = hProject hmap1 (HCons x0 (HCons x1 HNil))
hmap3 = fst $ prj hmap1 (HCons (Just ()) $ HCons (Just$ Just ()) HNil)
