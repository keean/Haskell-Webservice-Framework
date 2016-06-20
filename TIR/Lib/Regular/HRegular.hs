{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Main where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved. 
------------------------------------------------------------------------------

import Numeric
import Data.Typeable

------------------------------------------------------------------------------
-- generic product types.

data HNil = HNil deriving (Typeable,Show) -- otherwise known as "Unit"
newtype HUnit x = HUnit {hUnit::x} deriving (Typeable,Show)
data (x :*: y) = x :*: y deriving (Typeable,Show) -- binary product
data (l :@: t) = l :@: t deriving (Typeable,Show) -- constructor

infixr 2 :*:
infixr 1 :@:

------------------------------------------------------------------------------
-- class for regular types

class HRegular r where
instance HRegular HNil 
instance HRegular (HUnit e)
instance (HRegular l,HRegular r) => HRegular (l :*: r)
instance HRegular r => HRegular (x :@: r)

class HRegular r => ShowHRegular r where
	showHRegular :: r -> ShowS
instance (Show l,ShowHRegular r) => ShowHRegular (l:@:r) where
	showHRegular (l:@:r) = showChar '(' . shows l . showString "::" . showHRegular r . showChar ')'
instance (ShowHRegular l,ShowHRegular r) => ShowHRegular (l :*: r) where
	showHRegular (l :*: r) = showChar '(' . showHRegular l . showString ")(" . showHRegular r . showChar ')'
instance Show r => ShowHRegular (HUnit r) where
	showHRegular (HUnit r) = shows r
instance ShowHRegular HNil where
	showHRegular _ = showString "()"

-----------------------------------------------------------------------------
-- booleans

data HBoolFalse = HBoolFalse
data HBoolTrue = HBoolTrue

type HFalse = HBoolFalse :@: HNil
type HTrue = HBoolTrue :@: HNil

hFalse :: HFalse
hFalse = HBoolFalse :@: HNil

hTrue :: HTrue
hTrue = HBoolTrue :@: HNil

class HRegular b => HBool b where
	reflectHBool :: b -> Bool
instance HBool (HBoolFalse :@: HNil) where
	reflectHBool _ = False
instance HBool (HBoolTrue :@: HNil) where
	reflectHBool _ = True

showHBool :: HBool b => b -> ShowS
showHBool b = shows (reflectHBool b)

instance HBool (HBoolFalse :@: HNil) => Show (HBoolFalse :@: HNil) where
	showsPrec _ n = showHBool n
instance HBool (HBoolTrue :@: HNil) => Show (HBoolTrue :@: HNil) where
	showsPrec _ n = showHBool n

------------------------------------------------------------------------------
-- natural numbers

data HNatZero = HNatZero
data HNatSucc = HNatSucc

type Zero = HNatZero :@: HNil
type HSucc n = HNatSucc :@: n 

zero :: Zero
zero = HNatZero :@: HNil

hSucc :: n -> HSucc n
hSucc n = HNatSucc :@: n

hPrec :: HSucc n -> n
hPrec (HNatSucc :@: n) = n

class HRegular n => HNat n where
	reflectHNat :: Integral i => n -> i
instance HNat (HNatZero :@: HNil) where
	reflectHNat _ = 0
instance HNat n => HNat (HNatSucc :@: n) where
	reflectHNat (HNatSucc :@: n) = 1 + reflectHNat n

showHNat :: HNat n => n -> ShowS
showHNat n = showInt (reflectHNat n :: Integer)

instance HNat (HNatZero :@: HNil) => Show (HNatZero :@: HNil) where
	showsPrec _ n = showHNat n
instance HNat (HNatSucc :@: n) => Show (HNatSucc :@: n) where
	showsPrec _ n = showHNat n

type One = HSucc Zero
one :: One
one = hSucc zero
type Two = HSucc One
two :: Two
two = hSucc one
type Three = HSucc Two
three :: Three
three = hSucc two
type Four = HSucc Three
four :: Four
four = hSucc three
type Five = HSucc Four
five :: Five
five = hSucc four
type Six = HSucc Five
six :: Six
six = hSucc five
type Seven = HSucc Six
seven :: Seven
seven = hSucc six
type Eight = HSucc Seven
eight :: Eight
eight = hSucc seven
type Nine = HSucc Eight
nine :: Nine
nine = hSucc eight

------------------------------------------------------------------------------
-- a heterogeneous list

data HListCons = HListCons deriving (Typeable,Show)
data HListNil = HListNil deriving (Typeable,Show)

type HEmpty = HListNil :@: HNil
type HCons e l = HListCons :@: (HUnit e :*: l)

hEmpty :: HEmpty
hEmpty = HListNil :@: HNil

hCons :: HList l => e -> l -> HCons e l
hCons e l = HListCons :@: (HUnit e :*: l)

type e ::: l = HCons e l
(...) :: HList l => e -> l -> e ::: l
e ... l = hCons e l

infixr 1 :::
infixr 1 ...
                                                                                                                                               
class HRegular l => HList l
instance HList (HListNil :@: HNil)
instance HList l => HList (HListCons :@: (HUnit e :*: l))

type HSingleton e = e ::: HEmpty
hSingleton :: e -> HSingleton e
hSingleton e = e ... hEmpty

class HList l => ShowHList l where
	showHList' :: l -> ShowS
instance ShowHList (HListNil :@: HNil) where
	showHList' _ = showChar ']'
instance Show e => ShowHList (HListCons :@: HUnit e :*: (HListNil :@: HNil)) where
	showHList' (HListCons :@: HUnit e :*: (HListNil :@: HNil)) = shows e . showChar ']'
instance (Show e,ShowHList l) => ShowHList (HListCons :@: HUnit e :*: l) where
	showHList' (HListCons :@: HUnit e :*: l) = shows e . showChar ',' . showHList' l

showHList :: ShowHList l => l -> ShowS
showHList l = showString "HList [" . showHList' l

instance ShowHList (HListNil :@: l) => Show (HListNil :@: l) where
	showsPrec _ l = showHList l
instance ShowHList (HListCons :@: l) => Show (HListCons :@: l) where
	showsPrec _ l = showHList l

myList ::Int ::: String ::: String ::: Int ::: HEmpty
myList = 3 ... "aaa" ... "bbb" ... 6 ... hEmpty

------------------------------------------------------------------------------
-- a heterogeneous binary tree

data HTreeLeaf = HTreeLeaf
data HTreeBranch = HTreeBranch

type HLeaf x = HTreeLeaf :@: HUnit x
type HBranch x y = HTreeBranch :@: (x :*: y)

hLeaf :: x -> HTreeLeaf :@: HUnit x
hLeaf x = HTreeLeaf :@: HUnit x

hBranch :: (HTree x,HTree y) => x -> y -> HTreeBranch :@: x :*: y
hBranch x y = HTreeBranch :@: x :*: y

class HRegular t => HTree t 
instance HTree (HTreeLeaf :@: HUnit e)
instance (HTree x,HTree y) => HTree (HTreeBranch :@: x :*: y)

class HTree t => ShowHTree t where
	showHTree' :: t -> ShowS
instance Show x => ShowHTree (HTreeLeaf :@: HUnit x) where
	showHTree' (HTreeLeaf :@: HUnit x) = shows x
instance (ShowHTree x,ShowHTree y) => ShowHTree (HTreeBranch :@: (x:*:y)) where
	showHTree' (HTreeBranch :@: (x:*:y)) = showChar '[' . showHTree' x
		. showChar '|' . showHTree' y . showChar ']'

showHTree :: ShowHTree t => t -> ShowS
showHTree t = showString "HTree " . showHTree' t

instance ShowHTree (HTreeLeaf :@: l) => Show (HTreeLeaf :@: l) where
	showsPrec _ t = showHTree t
instance ShowHTree (HTreeBranch :@: l) => Show (HTreeBranch :@: l) where
	showsPrec _ t = showHTree t

myTree :: HBranch (HBranch (HLeaf String) (HLeaf Int)) (HBranch (HLeaf String) (HLeaf Int))
myTree = hBranch (hBranch (hLeaf "aaa") (hLeaf 5)) (hBranch (hLeaf "bbb") (hLeaf 4))

------------------------------------------------------------------------------
-- a heterogeneous rose tree

data HRoseNode = HRoseNode
data HRoseFork = HRoseFork
data HRoseNil = HRoseNil

type HNode e l = HRoseNode :@: (HUnit e :*: l)
type HForkCons r l = HRoseFork :@: (r :*: l)
type HForkNil = HRoseNil :@: HNil

hNode :: HRose' l => e -> l -> HRoseNode :@: (HUnit e :*: l)
hNode e l = HRoseNode :@: (HUnit e :*: l)

hForkCons :: (HRose r,HRose' l) => r -> l -> HRoseFork :@: (r :*: l)
hForkCons r l = HRoseFork :@: (r :*: l)

hForkNil :: HRoseNil :@: HNil
hForkNil = HRoseNil :@: HNil

type e :&: l = HForkCons e l
(.&.) :: (HRose e, HRose' l) => e -> l -> e :&: l
e .&. l = hForkCons e l

infixr 1 :&:
infixr 1 .&.

class HRegular t => HRose t
instance HRose' l => HRose (HRoseNode :@: (HUnit e :*: l))

class HRegular t => HRose' t
instance HRose' (HRoseNil :@: HNil)
instance (HRose r,HRose' l) => HRose' (HRoseFork :@: (r :*: l))

myRose :: HNode Int (HNode String HForkNil :&: HNode Int HForkNil :&: HNode String HForkNil :&: HForkNil)
myRose = hNode 7 (hNode "aaa" hForkNil .&. hNode 8 hForkNil .&. hNode "bbb" hForkNil .&. hForkNil)

------------------------------------------------------------------------------

class Apply t p a | t p -> a where
	apply :: t -> p -> a

------------------------------------------------------------------------------
-- (| generic fold for product types |)

class Catamorphism f r w | f r -> w where
	cata :: f -> r -> w
instance (Catamorphism f r w,Apply f (l:@:w) w') => Catamorphism f (l:@:r) w' where 
	cata f (l:@:r) = apply f (l:@:cata f r)
instance Apply f HNil w => Catamorphism f HNil w where
	cata f _ = apply f HNil
instance Apply f (HUnit r) w => Catamorphism f (HUnit r) w where
	cata f r = apply f r
instance (Catamorphism f l w,Catamorphism f r w',Apply f (w:*:w') w'') => Catamorphism f (l:*:r) w'' where
	cata f (l:*:r) = apply f (cata f l:*:cata f r)

------------------------------------------------------------------------------
-- {| paramorphism |}

class Paramorphism f r w | f r -> w where
	para :: f -> r -> w
instance (Paramorphism f r w,Apply f (l:@:(r,w)) w') => Paramorphism f (l:@:r) w' where
	para f (l:@:r) = apply f (l:@:(r,para f r))
instance Apply f HNil w => Paramorphism f HNil w where
	para f _ = apply f HNil
instance Apply f (HUnit r) w => Paramorphism f (HUnit r) w where
	para f r = apply f r
instance (Paramorphism f l w,Paramorphism f r w',Apply f ((l,w):*:(r,w')) w'') => Paramorphism f (l:*:r) w'' where
	para f (l:*:r) = apply f ((l,para f l):*:(r,para f r))

------------------------------------------------------------------------------
-- [| generic unfold for product types |]

class Anamorphism f v w | f v -> w where
	ana :: f -> v -> w
instance (Apply f u v,Anamorphism' f v w) => Anamorphism f u w where
	ana f v = ana' f (apply f v)

class Anamorphism' f x w | f x -> w where
	ana' :: f -> x -> w
instance Anamorphism' f r w => Anamorphism' f (l:@:r) (l:@:w) where
	ana' f (l:@:r) = (l:@:ana' f r)
instance Anamorphism' f HNil HNil where
	ana' _ _ = HNil
instance Anamorphism' f (HUnit u) (HUnit u) where
	ana' _ v = v
instance (Anamorphism' f u w,Anamorphism' f v w') => Anamorphism' f (u:*:v) (w:*:w') where
	ana' f (u:*:v) = ana' f u :*: ana' f v
instance Anamorphism f v w => Anamorphism' f v w where
	ana' f v = ana f v

------------------------------------------------------------------------------
-- [[ hylomorphism ]]

class Hylomorphism f v g w | f v g -> w where
	hylo :: f -> v -> g -> w
instance (Apply f u v,Hylomorphism' f v g w) => Hylomorphism f u g w where
	hylo f u g = hylo' f (apply f u) g

class Hylomorphism' f x g w | f x g -> w where
	hylo' :: f -> x -> g -> w
instance (Hylomorphism' f r g w,Apply g (l:@:w) w') => Hylomorphism' f (l:@:r) g w' where
   hylo' f (l:@:r) g = apply g (l:@:hylo' f r g) 
instance Apply g HNil w => Hylomorphism' f HNil g w  where
	hylo' _ _ g = apply g HNil
instance Apply g (HUnit v) w => Hylomorphism' f (HUnit v) g w where
	hylo' _ x g = apply g x
instance (Hylomorphism' f l g w,Hylomorphism' f r g w',Apply g (w:*:w') w'') => Hylomorphism' f (l:*:r) g w'' where
	hylo' f (l:*:r) g = apply g (hylo' f l g:*:hylo' f r g)
instance Hylomorphism f v g w => Hylomorphism' f v g w where
	hylo' f v g = hylo f v g

------------------------------------------------------------------------------
-- map

class HMap f r w | f r -> w where
	hMap :: f -> r -> w
instance HMap f r w => HMap f (l:@:r) (l:@:w) where
	hMap f (l:@:r) = l:@:hMap f r
instance HMap f HNil HNil where
	hMap _ _ = HNil
instance Apply f v w => HMap f (HUnit v) (HUnit w) where
	hMap f (HUnit v) = HUnit (apply f v)
instance (HMap f l w,HMap f r w') => HMap f (l:*:r) (w:*:w') where
	hMap f (l:*:r) = hMap f l :*: hMap f r

------------------------------------------------------------------------------
-- examples

newtype Sequence a = Sequence a
instance (Monad m,Apply f HNil (m a)) => Apply (Sequence f) HNil (m a) where
	apply (Sequence f) HNil = apply f HNil
instance (Monad m,Apply f (HUnit x) (m a)) => Apply (Sequence f) (HUnit x) (m a) where
	apply (Sequence f) x = apply f x
instance Monad m => Apply (Sequence f) (m a:*:m a) (m a) where
	apply _ (x:*:y) = do { x; y }
instance Monad m => Apply (Sequence f) (l:@:m a) (m a) where
	apply _ (_:@:x) = x

data HShow = HShow
instance Apply HShow HNil (IO ()) where
	apply _ _ = putStrLn $ "END"
instance (Typeable x, Show x) => Apply HShow (HUnit x) (IO ()) where
	apply _ (HUnit x) = putStrLn $ (shows x . showString "::" . shows (typeOf x)) ""

data HSum = HSum
instance Apply HSum HNil Int where
	apply _ _ = 0
instance Apply HSum (HUnit Int) Int where
	apply _ (HUnit i) = i
instance Apply HSum (HUnit e) Int where
	apply _ _ = 0
instance Apply HSum (Int:*:Int) Int where
	apply _ (i:*:j) = i+j
instance Apply HSum (l:@:Int) Int where
	apply _ (_:@:i) = i

data AnaList = AnaList
instance (HRegular x,Num y) => Apply AnaList (HNatSucc :@: x,y) (HListCons :@: HUnit y :*: (x,y)) where
	apply _ (x,y) = HListCons :@: HUnit y :*: (hPrec x,y+1)
instance Apply AnaList (HNatZero :@: HNil,y) (HListNil :@: HNil) where
	apply _ (_,_) = hEmpty

data AnaTree = AnaTree
instance (HRegular x,Num y) => Apply AnaTree (HNatSucc :@: x,y) (HTreeBranch :@: (x,y) :*: (x,y)) where
	apply _ (x,y) = HTreeBranch :@: (hPrec x,y) :*: (hPrec x,y+1)
instance Apply AnaTree (HNatZero :@: HNil,y) (HTreeLeaf :@: HUnit y) where
	apply _ (_,y) = hLeaf y

------------------------------------------------------------------------------
-- convert to list

data HConcat = HConcat
instance Apply HConcat (y,z) w => Apply HConcat (HListCons :@: HUnit x :*: y,z) (HListCons :@: HUnit x :*: w) where
	apply f (HListCons :@: x :*: y,z) = HListCons :@: x :*: apply f (y,z)
instance Apply HConcat (HListNil :@: HNil,z) z where
	apply _ (_,z) = z

data ToHList = ToHList
instance Apply ToHList HNil (HListNil :@: HNil) where
	apply _ _ = hEmpty
instance Apply ToHList (HUnit x) (HListCons :@: HUnit x :*: (HListNil :@: HNil)) where
	apply _ x = HListCons :@: x :*: (HListNil :@: HNil)
instance Apply HConcat (l,r) w => Apply ToHList (l:*:r) w where
	apply _ (l:*:r) = apply HConcat (l,r)
instance Apply ToHList (l:@:r) r where
	apply _ (_:@:r) = r

------------------------------------------------------------------------------
-- a test...

main :: IO ()
main = do
	cata (Sequence HShow) myList
	putStrLn ""
	cata (Sequence HShow) myTree
	putStrLn ""
	cata (Sequence HShow) myRose
	putStrLn ""
	putStrLn $ showInt (cata HSum myList) "\n"
	putStrLn $ showInt (cata HSum myTree) "\n"
	putStrLn $ showInt (cata HSum myRose) "\n"
	putStrLn ""
	print (ana AnaTree (three,1::Int))
	print (ana AnaList (three,1::Int))
	cata (Sequence HShow) (ana AnaTree (three,1::Int))
	putStrLn ""
	cata (Sequence HShow) (ana AnaList (three,1::Int))
	putStrLn ""
	hylo AnaTree (three,1::Int) (Sequence HShow)
	putStrLn ""
	hylo AnaList (three,1::Int) (Sequence HShow)
	putStrLn ""
	print (cata ToHList myList)
	print (cata ToHList myTree)
	print (cata ToHList myRose)

