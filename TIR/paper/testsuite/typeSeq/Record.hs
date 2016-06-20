{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
 
module Record where

import Data.Char
import Data.Typeable
import HType
import HBool
import HNat
import HList hiding ( Key,Name,Price,Animal,myAnimal )
import HArray
import HTypeDriven
import HZip
import TIP
import TIC


{-----------------------------------------------------------------------------}

-- Convenience notation

infixr 4 :=:
type l :=: v = (l,v)

infixr 4 .=.
l .=. v = (l,v)

emptyRecord  = mkRecord $ hZip HNil HNil

type SelfLabelled x = (HProxy x,x)
selfLabelled x = (hProxy x,x)


{-----------------------------------------------------------------------------}

-- Labels as triplets
--  ns     --- type for name space
--  dt     --- distinguished type
--  String --- to enable run-time name information

data Label ns dt = Label ns dt String


-- The first label in a name space

firstLabel :: String -> ns -> Label ns HZero
firstLabel str ns = Label ns HZero str


-- Generate next label

nextLabel :: HNat nat
          => String 
          -> Label ns nat
          -> Label ns (HSucc nat)

nextLabel str (Label ns nat _)
  = Label ns (HSucc nat) str


-- To be able to show labels

labelString :: Label ns dt -> String
labelString (Label _ _ str) = str


{-----------------------------------------------------------------------------}

-- Record types

newtype Record l = Record l

mkRecord :: ( HZip x y l
            , HTypeIndexed x 
            )
              => l -> Record l
mkRecord = Record

{-----------------------------------------------------------------------------}

-- A Show instance to appeal to normal records

instance ShowComps l => Show (Record l)
 where
  show (Record l) =  "Record{"
                  ++ showComps "" l
                  ++ "}"

class ShowComps l
 where
  showComps :: String -> l -> String

instance ShowComps HNil
 where
  showComps _ HNil = ""

instance (Show c, ShowComps t) => ShowComps (HCons (Label ns dt,c) t)
 where
  showComps comma (HCons (l,c) t)
     =  comma
     ++ labelString l
     ++ "="
     ++ show c
     ++ showComps "," t

instance (ShowLabel l, Show c, ShowComps t) => ShowComps (HCons (l,c) t)
 where
  showComps comma (HCons (l,c) t)
     =  comma
     ++ showLabel l
     ++ "="
     ++ show c
     ++ showComps "," t

class ShowLabel l
 where
  showLabel :: l -> String
                                                                               
instance ShowLabel x => ShowLabel (HProxy x)
 where
  showLabel (x::HProxy x) = showLabel (undefined::x)
                                                                               
instance Typeable x => ShowLabel x
 where
  showLabel = (\(x:xs) -> toLower x:xs)
            . reverse
            . takeWhile (not . (==) '.')
            . reverse
            . tyconString
            . typerepTyCon
            . typeOf


{-----------------------------------------------------------------------------}

-- Lifted hAppend

instance ( HAppend l l' l''
         , HZip x y l
         , HZip x' y' l'
         , HZip x'' y'' l''
         , HTypeIndexed x''
         )
           => HAppend (Record l) (Record l') (Record l'')
 where
  hAppend (Record l) (Record l') = mkRecord (hAppend l l')


{-----------------------------------------------------------------------------}

-- Lifted hLookup

instance ( HZip x y l
         , HType2HNat x xv n
         , HLookup y n yv
         )
           => HLookup (Record l) xv yv
 where
  hLookup (Record l) xv = yv
   where
    (x,y) = hUnzip l
    n     = hType2HNat x (hProxy xv)
    yv    = hLookup y n


{-----------------------------------------------------------------------------}

-- Lifted hDelete

instance ( HZip ls vs r
         , HType2HNat ls l n
         , HDelete ls n ls'
         , HDelete vs n vs'
         , HZip ls' vs' r'
         )
           => HDelete (Record r) l (Record r')
 where
  hDelete (Record r) l = Record (hZip ls' vs')
   where
    (ls,vs) = hUnzip r
    n       = hType2HNat ls (hProxy l)
    ls'     = hDelete ls n
    vs'     = hDelete vs n
 

{-----------------------------------------------------------------------------}

-- Lifted hUpdate

instance ( HZip x y l
         , HType2HNat x xv n
         , HUpdateTP y n yv
         )
           => HUpdate (Record l) (xv,yv)
 where
  hUpdate (Record l) (xv,yv) = Record (hZip x y')
   where
    (x,y) = hUnzip l
    n     = hType2HNat x (hProxy xv)
    y'    = hUpdateTP y n yv
 

{-----------------------------------------------------------------------------}

-- Extension for records

instance ( HZip xt yt l
         , HExtend xv xt x
         , HExtend yv yt y
         , HZip x y l'
         , HTypeIndexed x
         )
           => HExtend (xv,yv) (Record l) (Record l')
 where
  hExtend (xv,yv) (Record l) = mkRecord l'
   where
    (xt,yt) = hUnzip l
    x       = hExtend xv xt
    y       = hExtend yv yt
    l'      = hZip x y


{-----------------------------------------------------------------------------}

instance HProject (Record l) HNil (Record HNil)
 where 
  hProject _ _ = emptyRecord

instance ( HZip x y l
         , HType2HNat x xv n
         , HLookup y n yv
         , HProject (Record l) l' r
         , HExtend (xv,yv) r r'
         )
           => HProject (Record l) (HCons xv l') r'
 where
  hProject (Record l) (HCons xv l') = r'
   where
    (x,y) = hUnzip l
    n     = hType2HNat x (hProxy xv)
    yv    = hLookup y n
    r     = hProject (Record l) l'
    r'    = hExtend (xv,yv) r


{-----------------------------------------------------------------------------}

instance ( HZip x y l'
         , HProject (Record l) x (Record l')
         )
           => HSubType (Record l) (Record l')


{-----------------------------------------------------------------------------}

class HAdd r l v r' | r l v -> r'
 where
  hAdd :: r -> l -> v -> r'


{-----------------------------------------------------------------------------}

instance HAdd HNil l v (HCons (l,v) HNil)


{-----------------------------------------------------------------------------}

-- A little experiment in constraint propagation

myExtension a b r = hExtend (a,b) r

{-

Inferred type;
Looks good but is too general.

*Record> :t myExtension
myExtension :: forall l' l t1 t.
               (HExtend (t, t1) l l') =>
               t -> t1 -> l -> l'


Specialisation to record type;
Looks to intimidating with all the HConses.

myExtension :: HExtend (xv,yv) (Record l) (Record (HCons (xv, yv) l))
            => xv -> yv -> Record l -> Record (HCons (xv, yv) l)

-}


-- Shield class;
-- Choice of class commits to records;
-- but verbose types are delayed until instance selection.

class HExtend xy r r' 
   => ExtendRecord xy r r' | xy r -> r'
 where
  extendRecord :: xy -> r -> r'

instance HExtend (xv,yv) (Record l) (Record (HCons (xv, yv) l))
      => ExtendRecord (xv,yv) (Record l) (Record (HCons (xv, yv) l))
 where
  extendRecord xy r = hExtend xy r


-- A variation on myExtension

myExtension' xy r = extendRecord xy r

{-

*Record> :t myExtension'
myExtension' :: forall r' r xy.
                (ExtendRecord xy r r') =>
                xy -> r -> r'

*Record> :t myExtension' (Key .=. (88::Integer))
myExtension' (Key .=. (88::Integer)) :: forall r' r.
                                        (ExtendRecord (Key, Integer) r r') =>
                                        r -> r'

*Record> :t myExtension' (Key .=. (88::Integer)) emptyRecord
myExtension' (Key .=. (88::Integer)) emptyRecord :: Record
                                                        (HCons (Key, Integer)
HNil)

-}


{-----------------------------------------------------------------------------}

-- The fout-n-mouth name space

data FAM = FAM

-- Labels

key     = firstLabel "key"     FAM
name    = nextLabel  "name"    key
breed   = nextLabel  "breed"   name
price   = nextLabel  "price"   breed
disease = nextLabel  "disease" price
cured   = nextLabel  "cured"   disease

-- Sample record

myAnimal =  key   .=. (42::Integer)
        .*. name  .=. "Angus"
        .*. breed .=. Cow
        .*. price .=. (75.5::Float)
        .*. emptyRecord


-- Other labels

data Key     = Key       deriving (Show,Typeable)
data Name    = Name      deriving (Show,Typeable)
data Breed'  = Breed     deriving (Show,Typeable)
data Price   = Price     deriving (Show,Typeable)
data Disease = Disease   deriving (Show,Typeable)
data Cured   = Cured     deriving (Show,Typeable)


-- Sample record type

type Animal =  Record (   Key    :=: Integer
                      :*: Name   :=: String
                      :*: Breed' :=: Breed
                      :*: Price  :=: Float
                      :*: HNil
                      )


-- Sample record again

myAnimal' =  Key   .=. (42::Integer)
         .*. Name  .=. "Angus"
         .*. Breed .=. Cow
         .*. Price .=. (75.5::Float)
         .*. emptyRecord


-- Extensible records and their labels are first class

tagWithTrue label record
  = label .=. True .*. record


-- Rename the label of record

hRename l l' r@(Record _) = r''
 where
  v   = hLookup r l
  r'  = hDelete r l
  r'' = hExtend (l',v) r' 



-- Session transcript

{-

*Record> myAnimal ! price
75.5

*Record> myAnimal ! breed
Cow

*Record> myAnimal @@ price .=. 8.8
Record{key=42,name="Angus",breed=Cow,price=8.8}

*Record> disease .=. BSE .*. myAnimal
Record{disease=BSE,key=42,name="Angus",breed=Cow,price=75.5}

*Record> cured .=. True .*. myAnimal
Record{cured=True,key=42,name="Angus",breed=Cow,price=75.5}

*Record> tagWithTrue Cured myAnimal
Record{cured=True,key=42,name="Angus",breed=Cow,price=75.5}

*Record> :t tagWithTrue
tagWithTrue :: forall l' l t.
               (HExtend (t, Bool) l l') =>
               t -> l -> l'

-}

{-----------------------------------------------------------------------------}
