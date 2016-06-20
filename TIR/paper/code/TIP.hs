{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module TIP where

{- 

   (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

   Type-indexed products.

-}


import FakePrelude
import HType
import HList
import HArray
import HTypeDriven


{-----------------------------------------------------------------------------}

-- The newtype for type-indexed products

newtype TIP l = TIP l
        deriving (Read,Show)

mkTIP :: HTypeIndexed l => l -> TIP l
mkTIP = TIP

unTIP :: TIP l -> l
unTIP (TIP l) = l

emptyTIP :: TIP HNil
emptyTIP = mkTIP HNil


{-----------------------------------------------------------------------------}

-- Type-indexed type sequences

class HList l => HTypeIndexed l
instance HTypeIndexed HNil
instance ( HFreeType e l
         , HTypeIndexed l
         )
           => HTypeIndexed (HCons e l)


{-----------------------------------------------------------------------------}

-- HFreeType lifted to TIPs

instance HFreeType p l => HFreeType p (TIP l)


{-----------------------------------------------------------------------------}

-- Type-indexed extension

hExtend' e (TIP l) = mkTIP (HCons e l)

{-

Valid type I

hExtend' :: (HTypeIndexed l, HFreeType (HProxy e) l)
         => e -> TIP l -> TIP (HCons e l)  

Valid type II

*TIP> :t hExtend'
hExtend' :: forall l e.
            (HTypeIndexed (HCons e l)) =>
            e -> TIP l -> TIP (HCons e l)

-}


{-----------------------------------------------------------------------------}

-- Lift extension through HExtend

instance HTypeIndexed (HCons e l)
      => HExtend e (TIP l) (TIP (HCons e l))
 where
  hExtend = hExtend'


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- Lifting previous operations -----------------------------------------------


instance ( HAppend l l' l''
         , HTypeIndexed l''
         ) 
           => HAppend (TIP l) (TIP l') (TIP l'')
 where
  hAppend (TIP l) (TIP l') = mkTIP (hAppend l l')

instance ( HQualify u a q
         , HTypeIndexed u
         , HTypeIndexed q
         ) 
           => HQualify (TIP u) a (TIP q)
 where
  hQualify   (TIP u) a = mkTIP (hQualify u a)
  hUnqualify (TIP q) a = mkTIP (hUnqualify q a)

instance HLookup l (HProxy e) e
      => HLookup (TIP l) (HProxy e) e
 where
  hLookup (TIP l) = hLookup l

instance ( HDelete l (HProxy e) l'
         , HTypeIndexed l'
         )
           => HDelete (TIP l) (HProxy e) (TIP l')
 where
  hDelete (TIP l) p = TIP (hDelete l p)

instance ( HUpdate l e
         , HTypeIndexed l
         )
           => HUpdate (TIP l) e
 where
  hUpdate (TIP l) e = mkTIP (hUpdate l e)

instance ( HProject l il l'
         , HTypeIndexed l'
         )
           => HProject (TIP l) il (TIP l')
 where
  hProject (TIP l) il = mkTIP (hProject l il)

instance ( HSplit l il l' l''
         , HTypeIndexed l'
         , HTypeIndexed l''
         )
           => HSplit (TIP l) il (TIP l') (TIP l'')
 where
  hSplit (TIP l) = (\(x,y) -> (mkTIP x,mkTIP y)) . hSplit l

instance HOccursMany e l
      => HOccursMany e (TIP l)
 where
  hOccursMany = hOccursMany . unTIP

instance HOccursMany1 e l
      => HOccursMany1 e (TIP l)
 where
  hOccursMany1 = hOccursMany1 . unTIP

instance HOccursFst e l
      => HOccursFst e (TIP l)
 where
  hOccursFst = hOccursFst . unTIP

instance HOccursOpt e l
      => HOccursOpt e (TIP l)
 where
  hOccursOpt = hOccursOpt . unTIP

instance HOccurs''' e l
      => HOccurs e (TIP l)
 where
  hOccurs = hOccurs''' . unTIP


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- Complement of the HFreeType class

class HBoundType e r
instance HBoundType e (HCons e l)
instance HBoundType e l => HBoundType e (HCons e' l)


-- Class to compute a Boolean for "free type" status

class HBool b => HFreeTypeStatus e l b | e l -> b
instance HFreeTypeStatus e HNil HTrue
instance HFreeTypeStatus e (HCons e l) HFalse
instance HFreeTypeStatus e l b => HFreeTypeStatus e (HCons e' l) b


{-----------------------------------------------------------------------------}

-- Subtyping for TIPs

class HSubType l l'
 where 
  hSubType :: l -> l' -> ()
  hSubType _ _ = ()

instance HSubType (TIP l) (TIP HNil)
instance (HBoundType e l, HSubType (TIP l) (TIP l'))
      =>  HSubType (TIP l) (TIP (HCons e l'))


{-----------------------------------------------------------------------------}

-- A contribution by a reviewer
-- (supposed to challenge forgotten types)

tuple ::  (HOccurs y l', HDelete l (HProxy x) l', HOccurs x l) => l -> (x,y)
tuple r = let x  = hOccurs r
              s  = hDelete r (hProxy x)
              y  = hOccurs s
          in (x,y)

one :: Int
one = 1

inc :: Int -> Int
inc i = i+1

oneTrue :: TIP (Int :*: Bool :*: HNil)
oneTrue = mkTIP (one .*. True .*. HNil)

test1 = let (a,b) = tuple oneTrue  in (inc a, not b)

{-

This worked fine at all times.

*TIP> test1
(2,False)

-}

{-

This didn't work for the reviewer:

*TIP> not $ fst $ tuple oneTrue
 
<interactive>:1:
    No instance for (HOccurs y (HCons Int HNil))
      arising from use of `tuple' at <interactive>:1
    In the second argument of `($)', namely `tuple oneTrue'
    In the second argument of `($)', namely `fst $ (tuple oneTrue)'
    In the definition of `it': it = not $ (fst $ (tuple oneTrue))

This expression is not even typeable:

*TIP> :t not $ fst $ tuple oneTrue
 
No instance for (HOccurs' y (HCons Int HNil))
  arising from use of `tuple' at <No locn>
In the second argument of `($)', namely `tuple oneTrue'
In the second argument of `($)', namely `fst $ (tuple oneTrue)'

-}

{-

A revised hOccurs grounds its result type using the last list element.


*TIP> not $ fst $ tuple oneTrue
False

-}

tuple' :: (HOccurs x l,HDelete l (HProxy x) m,HOccurs y m,
   HOccurs y l,HDelete l (HProxy y) n,HOccurs x n) => l -> (x,y)
tuple' l = let
   x = hOccurs l
   m = hDelete l (hProxy x)
   y = hOccurs m
   in (x,y)


bigTIP :: TIP (Float :*: Int :*: Bool :*: String :*: HNil)
bigTIP = mkTIP (1 .*. 1 .*. True .*. "True" .*. HNil)

{-

One would expect the following to work:

*TIP> not $ fst $ tuple' bigTIP
 
<interactive>:1:
    No instances for ... 

-}


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

-- Sample code

myTipyCow = TIP myAnimal

animalKey :: (HOccurs Key l, HSubType l (TIP Animal))
          => l -> Key
animalKey = hOccurs


{-

*TIP> :t myTipyCow
myTipyCow :: TIP Animal

*TIP> hOccurs myTipyCow :: Breed
Cow

*TIP> hExtend BSE myTipyCow
TIP (HCons BSE 
    (HCons (Key 42)
    (HCons (Name "Angus")
    (HCons Cow
    (HCons (Price 75.5)
     HNil)))))

*TIP> BSE .*. myTipyCow
--- same as before ---

*TIP> Sheep .*. myTipyCow
Type error ...

*TIP> Sheep .*. hDelete myTipyCow (HProxy::HProxy Breed)
TIP (HCons Sheep (HCons (Key 42) (HCons (Name "Angus") (HCons (Price 75.5) HNil))))

*TIP> hUpdate myTipyCow Sheep
TIP (HCons (Key 42) (HCons (Name "Angus") (HCons Sheep (HCons (Price 75.5) HNil))))

-}

{-----------------------------------------------------------------------------}
