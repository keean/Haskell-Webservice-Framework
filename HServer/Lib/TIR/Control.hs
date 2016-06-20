{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Lib.TIR.Control where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved. 
------------------------------------------------------------------------------

import qualified Prelude

------------------------------------------------------------------------------

class Force a b | a -> b, b -> a where
   force :: a -> b
instance Force' () a b => Force a b where
   force x = force' () x
class Force' t a b | t a -> b, t b -> a where
   force' :: t -> a -> b 
instance Force'' t a b => Force' t a b where
   force' = force''
class Force'' t a b | t a -> b, t b -> a where
   force'' :: t -> a -> b 
instance Force'' () a a where
   force'' _ x = x

------------------------------------------------------------------------------
-- application and continuation. 

class Select t p a | t -> p a where
	select :: t -> p -> a
	
class Apply t p a | t p -> a where
	apply :: t -> p -> a

class Cont t p a where
	cont :: t -> p -> a

------------------------------------------------------------------------------

data Id = Id

instance Apply Id x x where
	apply _ x = x

------------------------------------------------------------------------------

newtype ConstFn a = ConstFn a

instance Apply (ConstFn z) a z where
	apply (ConstFn z) _ = z
instance Apply (ConstFn z) (a,b) z where
	apply (ConstFn z) _ = z
instance Apply (ConstFn z) (a,b,c) z where
	apply (ConstFn z) _ = z
instance Apply (ConstFn z) (a,b,c,d) z where
	apply (ConstFn z) _ = z
instance Apply (ConstFn z) (a,b,c,d,e) z where
	apply (ConstFn z) _ = z

------------------------------------------------------------------------------

newtype Sequence a = Sequence a

instance (Prelude.Monad m,Apply f x (m a)) => Apply (Sequence f) (x,m a) (m a) where
	apply (Sequence f) (x,c) = do { apply f x; c }

data SHOW = SHOW

instance Prelude.Show s => Apply SHOW s (Prelude.IO ()) where
	apply _ s = Prelude.print s

instance Prelude.Show s => Cont SHOW s (Prelude.IO ()) where
	cont _ s = Prelude.print s

------------------------------------------------------------------------------

class TypeCast x y where
	typeCast :: x -> Prelude.Maybe y
instance TypeCast x x where
	typeCast = Prelude.Just
instance TypeCast x y where
	typeCast _ = Prelude.Nothing

------------------------------------------------------------------------------

class ReflectTypeEq x y where
   reflectTypeEq :: x -> y -> Prelude.Bool
instance ReflectTypeEq x x where
   reflectTypeEq _ _ = Prelude.True
instance AssertTypeNe x y => ReflectTypeEq x y where
   reflectTypeEq _ _ = Prelude.False

------------------------------------------------------------------------------

class AssertFail a
data TypesEqual = TypesEqual
data TypesNotEqual = TypesNotEqual

class AssertTypeNe x y where
	assertTypeNe :: x -> y -> ()
instance AssertFail TypesEqual => AssertTypeNe x x where
	assertTypeNe _ _ = ()
instance AssertTypeNe x y where
	assertTypeNe _ _ = ()
	
class AssertTypeEq x y where
	assertTypeEq :: x -> y -> ()
instance AssertTypeEq x x where
	assertTypeEq _ _ = ()
instance AssertFail TypesNotEqual => AssertTypeEq x y where
	assertTypeEq _ _ = ()
	
------------------------------------------------------------------------------

typeId :: t -> t
typeId _ = Prelude.undefined

------------------------------------------------------------------------------

data HNothing = HNothing
newtype HJust a = HJust { hJust :: a } 

class HMaybe a
instance HMaybe HNothing
instance HMaybe (HJust a)

------------------------------------------------------------------------------

newtype HLeft a = HLeft { hLeft :: a } deriving Prelude.Show
newtype HRight a = HRight { hRight :: a} deriving Prelude.Show

class HEither a
instance HEither (HLeft a)
instance HEither (HRight a)

------------------------------------------------------------------------------
