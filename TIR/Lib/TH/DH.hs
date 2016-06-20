{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module DH where

import Numeric
import Char
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

--------------------------------------------------------------------

typeSig :: String -> [TypeQ] -> Q [Dec]
typeSig n ts = do
	t <- sigD (mkName n) (typeSig' n ts)
	return [t]

typeSig' :: String -> [TypeQ] -> TypeQ
typeSig' n (t0:ts@(_:_)) = appT (appT arrowT t0) (typeSig' n ts)
typeSig' _ (t0:_) = t0
typeSig' n _ = fail ("typeSig for function "++n++" is empty.")

--------------------------------------------------------------------

dataDec :: TypeQ -> Q [Dec]
dataDec t = do
	(ConT n) <- t
	d <- dataD (cxt []) n [] [] []
	return [d]

--------------------------------------------------------------------

reifyT :: String -> TypeQ
reifyT = conT . mkName

class Reify x where
   reifyType :: x -> TypeQ 
   reifyValue :: x -> TypeQ

--------------------------------------------------------------------
-- reifyType gives the LHS of the dependant type
-- reifyValue gives the RHS of the dependant type
--------------------------------------------------------------------
-- data Bool = BoolTrue | BoolFalse

data BoolTrue
data BoolFalse

boolType :: Bool
boolType = undefined

instance Reify Bool where
	reifyType _ = reifyT "Bool"
	reifyValue b = reifyT ((showString "Bool" . shows b) "")

--------------------------------------------------------------------
-- data Int (x::Int)= INeg x | I0 x | I1 x .. | INil

-- using decimal for readability, and to keep the context reduction
-- stack small.

data INil
data I0 x
data I1 x
data I2 x
data I3 x
data I4 x
data I5 x
data I6 x
data I7 x
data I8 x
data I9 x
data INeg x

intType :: Int
intType = undefined

instance Reify Int where
	reifyType _ = reifyT "Int"
	reifyValue i
		| i > 0 = appT (reifyPositive i) (reifyT "INil")
		| i < 0 = appT (reifyT "INeg") (appT (reifyPositive (-i)) (reifyT "INil"))
	   | otherwise = reifyT "INil"

reifyPositive :: Int -> TypeQ
reifyPositive i
	| i > 9 = appT (reifyPositive (i `div` 10)) (reifyT ((showString "I" . showInt (i `mod` 10)) ""))
	| otherwise = reifyT ((showString "I" . showInt (i `mod` 10)) "")

--------------------------------------------------------------------
-- data Char = C0 (x::Int) | C1 (x::Int) .. | CNil

data CNil
data C0 x
data C1 x
data C2 x
data C3 x
data C4 x
data C5 x
data C6 x
data C7 x
data C8 x
data C9 x

charType :: Char
charType = undefined

instance Reify Char where
	reifyType _ = reifyT "Char"
	reifyValue c = appT (reifyChar (ord c)) (reifyT "CNil")

reifyChar :: Int -> TypeQ
reifyChar c
	| c > 9 = appT (reifyChar (c `div` 10)) (reifyT ((showString "C" . showInt (c `div` 10)) ""))
	| otherwise = reifyT ((showString "C" . showInt (c `div` 10)) "")

--------------------------------------------------------------------
-- data List x = Cons x (List x) | Nil

data List x = Cons x (List x) | Nil
data ListCons x y = ListCons x y
data ListNil

instance Reify x => Reify (List x) where
	reifyType (_::List x) =  appT (reifyT "List") (reifyType (undefined::x))
	reifyValue l = nameList l

nameList :: Reify x => List x -> TypeQ
nameList (Cons x0 xs@(Cons _ _)) = appT (appT (reifyT "ListCons") (reifyValue x0)) (nameList xs)
nameList (Cons x0 _) = appT (appT (reifyT "ListCons") (reifyValue x0)) (reifyT "ListNil")
nameList Nil = reifyT "ListNil"

--------------------------------------------------------------------
-- data OrdList x = OrdCons (a::x) (b::OrdList x) where a > head b
--    | OrdNil

data OrdList x = OrdCons x (OrdList x) | OrdNil
data OrdListCons x y = OrdListCons x y
data OrdListNil 

orderedList :: Ord x => List x -> OrdList x
orderedList (Cons a l@(Cons b _))
	| a > b = OrdCons a (orderedList l)
	| otherwise = error "cannot convert unordered list to OrderedList"
orderedList (Cons a Nil) = OrdCons a OrdNil
orderedList _ = OrdNil

instance Reify x => Reify (OrdList x) where
	reifyType x = appT (reifyT "OrdList") (reifyType x)
	reifyValue x = nameOrdList x

nameOrdList :: Reify x => OrdList x -> TypeQ
nameOrdList (OrdCons x0 xs@(OrdCons _ _)) = appT (appT (reifyT "OrdListCons") (reifyValue x0)) (nameOrdList xs)
nameOrdList (OrdCons x0 _) = appT (appT (reifyT "OrdListCons") (reifyValue x0)) (reifyT "OrdListNil")
nameOrdList OrdNil = reifyT "OrdListNil"

--------------------------------------------------------------------
-- 				  (type::kind)	 		 (value::type)
-- data LessThan (a::Int) = LessThan (b::Int) where b < a

data LessThanKind = LessThanType Int
data LessThanType a = LessThan Int

lessThanType :: LessThanKind -> LessThanType a -> LessThanKind
lessThanType (LessThanType n) (LessThan i)
	| i < n = LessThanType n
	| otherwise = error "cannot convert to LessThan"

instance Reify LessThanKind where
	reifyType (LessThanType _) = reifyT "LessThanKind"
	reifyValue (LessThanType i) = appT (reifyT "LessThanType") (reifyValue i)

--------------------------------------------------------------------
-- data VectorType (n::Int) x = Vector [x] where n == length x

data VectorKind x = VectorType Int x
data VectorType n x = Vector [x]

vectorType :: VectorKind t -> VectorType n t -> VectorKind t
vectorType (VectorType n t) (Vector v) 
	| length v == n = VectorType n t
	| otherwise = error "length does not match"

instance Reify t => Reify (VectorKind t) where
	reifyType (VectorType _ v) = appT (reifyT "VectorKind") (reifyType v)
	reifyValue (VectorType n v) = appT (appT (reifyT "VectorType") (reifyValue n)) (reifyType v)

