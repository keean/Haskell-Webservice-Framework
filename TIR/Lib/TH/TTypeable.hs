{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module TTypeable where

import Char 
import Numeric
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Control.Monad

------------------------------------------------------------------------------
-- primitive type level logic

data TTrue
data TFalse

instance Show TTrue where
	showsPrec _ _ = showString "TTrue"
instance Show TFalse where
	showsPrec _ _ = showString "TFalse"

ttrue :: TTrue
ttrue = undefined

tfalse :: TFalse
tfalse = undefined

class TBool t where
	reflectBool :: t -> Bool
instance TBool TTrue where
	reflectBool _ = True
instance TBool TFalse where
	reflectBool _ = False

class TNot t u | t -> u, u -> t where
	tNot :: t -> u
instance TNot TTrue TFalse where
	tNot _ = tfalse
instance TNot TFalse TTrue where
	tNot _ = ttrue

class TAnd t u v | t u -> v where
	tAnd :: t -> u -> v
instance TAnd TTrue TTrue TTrue where
	tAnd _ _ = ttrue
instance TAnd TFalse TTrue TFalse where
	tAnd _ _ = tfalse
instance TAnd TTrue TFalse TFalse where
	tAnd _ _ = tfalse
instance TAnd TFalse TFalse TFalse where
	tAnd _ _ = tfalse

class TOr t u v | t u -> v where
	tOr :: t -> u -> v
instance TOr TTrue TTrue TTrue where
	tOr _ _ = ttrue
instance TOr TFalse TTrue TTrue where
	tOr _ _ = ttrue
instance TOr TTrue TFalse TTrue where
	tOr _ _ = ttrue
instance TOr TFalse TFalse TFalse where
	tOr _ _ = tfalse

------------------------------------------------------------------------------

data NNil
data N0 n
data N1 n
data N2 n
data N3 n
data N4 n
data N5 n
data N6 n
data N7 n
data N8 n
data N9 n

nnil :: NNil
nnil = undefined
n0 :: x -> N0 x
n0 _ = undefined
n1 :: x -> N1 x
n1 _ = undefined
n2 :: x -> N2 x
n2 _ = undefined
n3 :: x -> N3 x
n3 _ = undefined
n4 :: x -> N4 x
n4 _ = undefined
n5 :: x -> N5 x
n5 _ = undefined
n6 :: x -> N6 x
n6 _ = undefined
n7 :: x -> N7 x
n7 _ = undefined
n8 :: x -> N8 x
n8 _ = undefined
n9 :: x -> N9 x
n9 _ = undefined

unN0 :: N0 x -> x
unN0 = undefined
unN1 :: N1 x -> x
unN1 = undefined
unN2 :: N2 x -> x
unN2 = undefined
unN3 :: N3 x -> x
unN3 = undefined
unN4 :: N4 x -> x
unN4 = undefined
unN5 :: N5 x -> x
unN5 = undefined
unN6 :: N6 x -> x
unN6 = undefined
unN7 :: N7 x -> x
unN7 = undefined
unN8 :: N8 x -> x
unN8 = undefined
unN9 :: N9 x -> x
unN9 = undefined

class TNat n where
	reflectTNat' :: Integral i => n -> i -> i
instance TNat NNil where
	reflectTNat' _ i = i
instance TNat n => TNat (N0 n) where
	reflectTNat' n i = reflectTNat' (unN0 n) (10*i)
instance TNat n => TNat (N1 n) where
	reflectTNat' n i = reflectTNat' (unN1 n) (10*i+1)
instance TNat n => TNat (N2 n) where
	reflectTNat' n i = reflectTNat' (unN2 n) (10*i+2)
instance TNat n => TNat (N3 n) where
	reflectTNat' n i = reflectTNat' (unN3 n) (10*i+3)
instance TNat n => TNat (N4 n) where
	reflectTNat' n i = reflectTNat' (unN4 n) (10*i+4)
instance TNat n => TNat (N5 n) where
	reflectTNat' n i = reflectTNat' (unN5 n) (10*i+5)
instance TNat n => TNat (N6 n) where
	reflectTNat' n i = reflectTNat' (unN6 n) (10*i+6)
instance TNat n => TNat (N7 n) where
	reflectTNat' n i = reflectTNat' (unN7 n) (10*i+7)
instance TNat n => TNat (N8 n) where
	reflectTNat' n i = reflectTNat' (unN8 n) (10*i+8)
instance TNat n => TNat (N9 n) where
	reflectTNat' n i = reflectTNat' (unN9 n) (10*i+9)

reflectTNat :: (TNat n,Integral i) => n -> i
reflectTNat n = reflectTNat' n 0

integer :: Integer -> Integer
integer = id

instance Show NNil where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N0 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N1 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N2 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N3 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N4 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N5 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N6 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N7 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N8 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)
instance TNat n => Show (N9 n) where
	showsPrec _ n = showInt (integer $ reflectTNat n)

class TNatEq n m t | n m -> t where
	tNatEq :: n -> m -> t
instance TNatEq NNil NNil TTrue where
	tNatEq _ _ = ttrue
instance TNatEq (N0 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N0 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N0 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N0 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N0 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N0 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N0 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N0 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N0 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N1 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N2 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N3 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N4 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N5 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N6 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N7 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N8 n) (N9 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N0 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N1 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N2 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N3 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N4 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N5 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N6 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N7 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq (N9 n) (N8 m) TFalse where
	tNatEq _ _ = tfalse
instance TNatEq n m t => TNatEq (N0 n) (N0 m) t where
	tNatEq n m = tNatEq (unN0 n) (unN0 m) 
instance TNatEq n m t => TNatEq (N1 n) (N1 m) t where
	tNatEq n m = tNatEq (unN1 n) (unN1 m) 
instance TNatEq n m t => TNatEq (N2 n) (N2 m) t where
	tNatEq n m = tNatEq (unN2 n) (unN2 m) 
instance TNatEq n m t => TNatEq (N3 n) (N3 m) t where
	tNatEq n m = tNatEq (unN3 n) (unN3 m) 
instance TNatEq n m t => TNatEq (N4 n) (N4 m) t where
	tNatEq n m = tNatEq (unN4 n) (unN4 m) 
instance TNatEq n m t => TNatEq (N5 n) (N5 m) t where
	tNatEq n m = tNatEq (unN5 n) (unN5 m) 
instance TNatEq n m t => TNatEq (N6 n) (N6 m) t where
	tNatEq n m = tNatEq (unN6 n) (unN6 m) 
instance TNatEq n m t => TNatEq (N7 n) (N7 m) t where
	tNatEq n m = tNatEq (unN7 n) (unN7 m) 
instance TNatEq n m t => TNatEq (N8 n) (N8 m) t where
	tNatEq n m = tNatEq (unN8 n) (unN8 m) 
instance TNatEq n m t => TNatEq (N9 n) (N9 m) t where
	tNatEq n m = tNatEq (unN9 n) (unN9 m) 

------------------------------------------------------------------------------

data Label x = Label x deriving Show

------------------------------------------------------------------------------
-- label generation

nName :: Integer -> Name
nName n
	| n == 9 = ''N9
	| n == 8 = ''N8
	| n == 7 = ''N7
	| n == 6 = ''N6
	| n == 5 = ''N5
	| n == 4 = ''N4
	| n == 3 = ''N3
	| n == 2 = ''N2
	| n == 1 = ''N1
	| otherwise = ''N0

ctoi :: Char -> Integer
ctoi = toEnum . fromEnum

hash :: String -> Integer -> Integer
hash (c0:cs) i = hash cs $ 64 * i + case c0 of
	c	| c >= '0' && c <= '9' -> ctoi c - 48
		| c >= 'A' && c <= 'Z' -> ctoi c - 55 
		| c >= 'a' && c <= 'z' -> ctoi c - 61 
		| c == '\'' -> 62
		| otherwise -> 63
hash _ i = i

itot :: Integer -> TypeQ -> TypeQ
itot i n 
	| i > 0 = case quotRem i 10 of
		(q,r) -> itot q (appT (conT (nName r)) n)
	| otherwise = n

labelHash :: String -> TypeQ
labelHash l = itot (hash l 0) (conT ''NNil)

label :: String -> Q [Dec]
label l = do
	m <- qualifyString l
	s <- sigD (gName m) $ appT (conT 'Label) (labelHash m)
	f <- funD (gName m) [clause [] (normalB $ varE $ 'undefined) []]
	return [s,f]
	where
		
		gName :: String -> Name
		gName m = mkNameG_v m l

------------------------------------------------------------------------------

data TNil
data TCons a b

class ShowTList l where
	showTList :: l -> String -> String
instance ShowTList TNil where
	showTList _ = id
instance (Show a,TTypeable a b) => ShowTList (TCons a TNil) where
	showTList l = let (h::a) = (thead l) in shows h 
instance (ShowTList (TCons b l),Show a,TTypeable a c) => ShowTList (TCons a (TCons b l)) where
	showTList l = let (h::a) = (thead l) in shows h . showString ", " . showTList (ttail l)

instance Show TNil where
	showsPrec _ _ = id
instance ShowTList (TCons a l) => Show (TCons a l) where
	showsPrec _ l = showString "{:" . showTList l . showString ":}"

tnil :: TNil
tnil = undefined

tcons :: a -> b -> TCons a b
tcons _ _ = undefined

thead :: TCons a b -> a
thead _ = undefined

ttail :: TCons a b -> b
ttail _ = undefined

data TRep n t
instance (Show n,Show t) => Show (TRep n t) where
	showsPrec _ _ = shows (undefined::n) . shows (undefined::t)
tTyRep :: TRep n t -> n
tTyRep _ = undefined
tTyVars :: TRep n t -> t
tTyVars _ = undefined

class TList t
instance TList TNil
instance (TType t,TList l) => TList (TCons t l)
class TType t
instance (TNat n,TList t) => TType (TRep n t)

class THeadEq t u v | t u -> v where
	theadEq :: t -> u -> v
instance TNatEq n m z => THeadEq (TRep n t) (TRep m u) z where
	theadEq t u = tNatEq (tTyRep t :: n) (tTyRep u :: m)

class TTypeEq t u v | t u -> v where
	ttypeEq :: t -> u -> v
instance (TNatEq n m v,TListEq t u w,TAnd v w z) => TTypeEq (TRep n t) (TRep m u) z where
	ttypeEq t u = (tNatEq (tTyRep t :: n) (tTyRep u :: m) :: v)
		 	 `tAnd` (tlistEq (tTyVars t :: t) (tTyVars u :: u) :: w)

class TListEq t u v | t u -> v where
	tlistEq :: t -> u -> v
instance TListEq TNil TNil TTrue where
	tlistEq _ _ = ttrue
instance TListEq TNil (TCons u m) TFalse where
	tlistEq _ _ = tfalse
instance TListEq (TCons t l) TNil TFalse where
	tlistEq _ _ = tfalse
instance (TTypeEq t u v,TListEq l m w,TAnd v w z) => TListEq (TCons t l) (TCons u m) z where
	tlistEq t u = (ttypeEq (thead t :: t) (thead u :: u) :: v)
			 `tAnd` (tlistEq (ttail t :: l) (ttail u :: m) :: w)

class TTypeable a b | a -> b where
	ttypeOf :: a -> b
	ttypeOf _ = undefined
	ttypeName :: a -> String

class TypeEq x y z | x y -> z where
	typeEq :: x -> y -> z
instance (TTypeable x tx,TTypeable y ty,TTypeEq tx ty z) => TypeEq x y z where
	typeEq x y = ttypeEq (ttypeOf x :: tx) (ttypeOf y :: ty)

class HeadEq x y z | x y -> z where
	headEq :: x -> y -> z
instance (TTypeable x tx,TTypeable y ty,THeadEq tx ty z) => HeadEq x y z where
	headEq x y = theadEq (ttypeOf x :: tx) (ttypeOf y :: ty)

------------------------------------------------------------------------------
-- deriving ttypeable

hlistTyVars :: [Name] -> TypeQ
hlistTyVars (n:ns) = appT (appT (conT ''TCons) (varT n)) (hlistTyVars ns)
hlistTyVars _ = conT ''TNil

applyTyVars :: TypeQ -> [Name] -> TypeQ
applyTyVars q (v0:vs) = appT (applyTyVars q vs) (varT v0)
applyTyVars q _ = q

instanceHead :: Name -> [Name] -> [Name] -> TypeQ
instanceHead n u v = appT (appT (conT ''TTypeable) (applyTyVars (conT n) $ reverse u)) $
	appT (appT (conT ''TRep) (labelHash $ show n)) (hlistTyVars v)

applyToNil :: Int -> Name -> TypeQ
applyToNil i n
	| i>0 = appT (applyToNil (i-1) n) (conT ''TNil)
	| otherwise = varT n

typeHead :: Int -> Name -> TypeQ
typeHead i n
	| i>0 = do
		m <- (newName . nameBase) n
		appT (appT (conT ''TCons) (varT n)) (varT m)
	| otherwise = varT n

constraints :: [Int] -> [Name] -> [Name] -> [TypeQ]
constraints (t0:ts) (u0:us) (v0:vs) = appT (appT (conT ''TTypeable) (applyToNil t0 u0)) (typeHead t0 v0) : constraints ts us vs
constraints _ _ _ = []

findArity :: Name -> Type -> Maybe (Bool,Int)
findArity n (VarT m)
	| n==m = Just (True,0)
	| otherwise = Nothing
findArity n (AppT t u) = case findArity n t of
	Just (True,v) -> Just (True,v+1)
	Just (_,v) -> Just (False,v)
	_ -> case findArity n u of
		Just (_,v) -> Just (False,v)
		_ -> Nothing
findArity _ _ = Nothing

findArityMap :: (a -> Type) -> Name -> [a] -> Maybe Int
findArityMap f n (t:ts) = case findArity n (f t) of
	Just (_,v) -> Just v
	_ -> findArityMap f n ts
findArityMap _ _ _ = Nothing

findArityCon :: Name -> [Con] -> Int
findArityCon n (NormalC _ t:cs) = case findArityMap snd n t of
	Just v -> v
	_ -> findArityCon n cs
findArityCon n (RecC _ t:cs) = case findArityMap (\(_,_,a) -> a) n t of
	Just v -> v
	_ -> findArityCon n cs
findArityCon n (InfixC (_,t) _ (_,u):cs) = case findArity n t of
	Just (_,v) -> v
	_ -> case findArity n u of
		Just (_,w) -> w
		_ -> findArityCon n cs
findArityCon _ _ = 0

deriveInstance :: Name -> [Name] -> [Con] -> Q Dec
deriveInstance name vars typ = let t = map (\a -> findArityCon a typ) vars in do
	m <- qualifyString (nameBase name)
	u <- sequence $ map (newName . nameBase) vars
	v <- sequence $ map (newName . nameBase) vars
	w <- instanceD (cxt $ constraints t u v) (instanceHead name u v) [
		funD (mkName "ttypeName") [clause [wildP] (normalB $ stringE m) []]]
	return w

ttypeable :: Name -> Q [Dec]
ttypeable d = do
	r <- reify d
	w <- case r of
		(TyConI t) -> do
			t' <- ttypeableMapName return t
			qReport False $ pprint t'
			return t'
		(PrimTyConI n _ _) | n == ''(->) -> do
			a <- newName "a"
			b <- newName "b"
			t <- deriveInstance ''(->) [a,b] [NormalC ''(->)
				[(NotStrict,AppT (AppT (ConT ''(->)) (VarT a)) (VarT b))]]
			return [t]
		_ -> do
			qReport True $ showString "ttypeable: " . shows d $ " is not a data declaration."
			return []
	return w

ttypeableMapName :: (Name -> Q Name) -> Dec -> Q [Dec]
ttypeableMapName f (DataD _ n v t _) = do
	m <- f n
	d <- deriveInstance m v t
	return [d]
ttypeableMapName f (NewtypeD _ n v t _) = do
	m <- f n
	d <- deriveInstance m v [t]
	return [d]
ttypeableMapName _ d = do
	qReport True $ showString "ttypeable: " . shows d $ " is not a data declaration."
	return []

qualifyName :: Name -> Q Name
qualifyName n = do
	m <- (qualifyString . nameBase) n
	return (mkName m)

qualifyString :: String -> Q String
qualifyString n = do
	m <- qCurrentModule
	return (m++('.':n))

