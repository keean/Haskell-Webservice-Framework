{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module Lib.TIR.Logic where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved. 
------------------------------------------------------------------------------
-- Modal logic for partial systems.

import qualified Prelude
import Lib.TIR.Control

------------------------------------------------------------------------------

data AllTrue = AllTrue
data SomeTrue = True | NotTrue
data SomeFalse = False | NotFalse
data AllFalse = AllFalse

class Modal m => MBool m
instance MBool AllTrue
instance MBool AllFalse

------------------------------------------------------------------------------

class ReifyBool t w where
	reifyBool :: Prelude.Bool -> t -> w
instance (Apply t AllTrue w,Apply t AllFalse w) => ReifyBool t w where
	reifyBool Prelude.True t = apply t AllTrue
	reifyBool Prelude.False t = apply t AllFalse

class Modal t where
	reflectBool :: t -> Prelude.Bool
	showModal :: t -> Prelude.ShowS
instance Modal AllTrue where
	reflectBool _ = Prelude.True
	showModal _ = Prelude.showString "AllTrue::AllTrue"
instance Modal SomeTrue where
	reflectBool True = Prelude.True
	reflectBool NotTrue = Prelude.False
	showModal True = Prelude.showString "True::SomeTrue"
	showModal NotTrue = Prelude.showString "NotTrue::SomeTrue"
instance Modal SomeFalse where
	reflectBool False = Prelude.False
	reflectBool NotFalse = Prelude.True
	showModal False = Prelude.showString "False::SomeFalse"
	showModal NotFalse = Prelude.showString "NotFalse::SomeFalse"
instance Modal AllFalse where
	reflectBool _ = Prelude.False
	showModal _ = Prelude.showString "AllFalse::AllFalse"

instance Modal m => Prelude.Show m where
	showsPrec _ m = showModal m

toSomeTrue :: Prelude.Bool -> SomeTrue
toSomeTrue b = if b then True else NotTrue

toSomeFalse :: Prelude.Bool -> SomeFalse
toSomeFalse b = if b then NotFalse else False

------------------------------------------------------------------------------

class Modal m => NecessarilyTrue m n | m -> n where
	necessarilyTrue :: m -> n
instance NecessarilyTrue AllTrue AllTrue where
	necessarilyTrue _ = force AllTrue
instance NecessarilyTrue SomeTrue AllFalse where
	necessarilyTrue _ = force AllFalse
instance NecessarilyTrue SomeFalse AllFalse where
	necessarilyTrue _ = force AllFalse
instance NecessarilyTrue AllFalse AllFalse where
	necessarilyTrue _ = force AllFalse

class Modal m => NecessarilyFalse m n | m -> n where
	necessarilyFalse :: m -> n
instance NecessarilyFalse AllTrue AllFalse where
	necessarilyFalse _ = AllFalse
instance NecessarilyFalse SomeTrue AllFalse where
	necessarilyFalse _ = AllFalse
instance NecessarilyFalse SomeFalse AllFalse where
	necessarilyFalse _ = AllFalse
instance NecessarilyFalse AllFalse AllTrue where
	necessarilyFalse _ = AllTrue

class Modal m => PossiblyTrue m n | m -> n where
	possiblyTrue :: m -> n
instance PossiblyTrue AllTrue AllFalse where
	possiblyTrue _ = AllFalse
instance PossiblyTrue SomeTrue AllTrue where
	possiblyTrue _ = AllTrue
instance PossiblyTrue SomeFalse AllFalse where
	possiblyTrue _ = AllFalse
instance PossiblyTrue AllFalse AllFalse where
	possiblyTrue _ = AllFalse

class Modal m => PossiblyFalse m n | m -> n where
	possiblyFalse :: m -> n
instance PossiblyFalse AllTrue AllFalse where
	possiblyFalse _ = AllFalse
instance PossiblyFalse SomeTrue AllFalse where
	possiblyFalse _ = AllFalse
instance PossiblyFalse SomeFalse AllTrue where
	possiblyFalse _ = AllTrue
instance PossiblyFalse AllFalse AllFalse where
	possiblyFalse _ = AllFalse

------------------------------------------------------------------------------

class MBool t => Conditional t x y z | t x y -> z where
	cond :: t -> x -> y -> z
instance Conditional AllFalse x y y where
	cond AllFalse _ y = y
instance Conditional AllTrue x y x where
	cond AllTrue x _ = x

------------------------------------------------------------------------------

class MBool a => AssertTrue a b where
	assertTrue :: a -> b -> ()
instance AssertTrue AllTrue b where
	assertTrue _ _ = ()
instance AssertFail b => AssertTrue AllFalse b where
	assertTrue _ _ = ()

class MBool a => AssertFalse a b where
	assertFalse :: a -> b -> ()
instance AssertFalse AllFalse b where
	assertFalse _ _ = ()
instance AssertFail b => AssertFalse AllTrue b where
	assertFalse _ _ = ()

------------------------------------------------------------------------------

class Modal m => Not m n | m -> n, n -> m where
	not :: m -> n
instance Not AllTrue AllFalse where
	not _ = AllFalse
instance Not SomeTrue SomeFalse where
	not True = False
	not NotTrue = NotFalse
instance Not SomeFalse SomeTrue where
	not False = True
	not NotFalse = NotTrue
instance Not AllFalse AllTrue where
	not _ = AllTrue

------------------------------------------------------------------------------

class Modal m => Determined m n | m -> n where
	determined :: m -> n
instance Determined AllTrue AllTrue where
	determined _ = AllTrue
instance Determined SomeTrue AllFalse where
	determined _ = AllFalse
instance Determined SomeFalse AllFalse where
	determined _ = AllFalse
instance Determined AllFalse AllTrue where
	determined _ = AllTrue

------------------------------------------------------------------------------

infixl 8 `eq`
class (Modal m,Modal n) => Eq m n o | m n -> o where
	eq :: m -> n -> o
instance Eq AllTrue AllTrue AllTrue where
	eq _ _ = AllTrue
instance Eq AllTrue SomeTrue AllFalse where
	eq _ _ = AllFalse
instance Eq AllTrue SomeFalse AllFalse where
	eq _ _ = AllFalse
instance Eq AllTrue AllFalse AllFalse where
	eq _ _ = AllFalse

instance Eq SomeTrue AllTrue AllFalse where
	eq _ _ = AllFalse
instance Eq SomeTrue SomeTrue SomeTrue where
	eq True True = True
	eq NotTrue NotTrue = True
	eq _ _ = NotTrue
instance Eq SomeTrue SomeFalse AllFalse where
	eq _ _ = AllFalse
instance Eq SomeTrue AllFalse AllFalse where
	eq _ _ = AllFalse

instance Eq SomeFalse AllTrue AllFalse where
	eq _ _ = AllFalse
instance Eq SomeFalse SomeTrue AllFalse where
	eq _ _ = AllFalse
instance Eq SomeFalse SomeFalse SomeFalse where
	eq False False = NotFalse
	eq NotFalse NotFalse = NotFalse
	eq _ _ = False
instance Eq SomeFalse AllFalse AllFalse where
	eq _ _ = AllFalse

instance Eq AllFalse AllTrue AllFalse where
	eq _ _ = AllFalse
instance Eq AllFalse SomeTrue AllFalse where
	eq _ _ = AllFalse
instance Eq AllFalse SomeFalse AllFalse where
	eq _ _ = AllFalse
instance Eq AllFalse AllFalse AllTrue where
	eq _ _ = AllTrue

------------------------------------------------------------------------------

infixl 4 `and`
class (Modal m,Modal n) => And m n o | m n -> o where
	and :: m -> n -> o
instance And AllTrue AllTrue AllTrue where
	and _ _ = AllTrue
instance And AllTrue SomeTrue SomeTrue where
	and _ n = n
instance And AllTrue SomeFalse SomeFalse where
	and _ n = n
instance And AllTrue AllFalse AllFalse where
	and _ _ = AllFalse

instance And SomeTrue AllTrue SomeTrue where
	and m _ = m
instance And SomeTrue SomeTrue SomeTrue where
	and True True = True
	and _ _ = NotTrue
instance And SomeTrue SomeFalse SomeFalse where
	and True NotFalse = NotFalse
	and _ _ = False
instance And SomeTrue AllFalse AllFalse where
	and _ _ = AllFalse

instance And SomeFalse AllTrue SomeFalse where
	and m _ = m
instance And SomeFalse SomeTrue SomeFalse where
	and NotFalse True = NotFalse
	and _ _ = False
instance And SomeFalse SomeFalse SomeFalse where
	and NotFalse NotFalse = NotFalse
	and _ _ = False
instance And SomeFalse AllFalse AllFalse where
	and _ _ = AllFalse

instance And AllFalse AllTrue AllFalse where
	and _ _ = force AllFalse
instance And AllFalse SomeTrue AllFalse where
	and _ _ = AllFalse
instance And AllFalse SomeFalse AllFalse where
	and _ _ = AllFalse
instance And AllFalse AllFalse AllFalse where
	and _ _ = AllFalse

------------------------------------------------------------------------------

infixl 4 `or`
class (Modal m,Modal n) => Or m n o | m n -> o where
	or :: m -> n -> o
instance Or AllTrue AllTrue AllTrue where
	or _ _ = force AllTrue
instance Or AllTrue SomeTrue AllTrue where
	or _ _ = AllTrue
instance Or AllTrue SomeFalse AllTrue where
	or _ _ = AllTrue
instance Or AllTrue AllFalse AllTrue where
	or _ _ = AllTrue

instance Or SomeTrue AllTrue AllTrue where
	or _ _ = AllTrue
instance Or SomeTrue SomeTrue SomeTrue where
	or True _ = True
	or _ True = True
	or _ _ = NotTrue
instance Or SomeTrue SomeFalse SomeTrue where
	or True _ = True
	or _ NotFalse = True
	or _ _ = NotTrue
instance Or SomeTrue AllFalse SomeTrue where
	or m _ = m

instance Or SomeFalse AllTrue AllTrue where
	or _ _ = AllTrue
instance Or SomeFalse SomeTrue SomeTrue where
	or NotFalse _ = True
	or _ True = True
	or _ _ = NotTrue
instance Or SomeFalse SomeFalse SomeFalse where
	or NotFalse _ = NotFalse
	or _ NotFalse  = NotFalse
	or _ _ = False
instance Or SomeFalse AllFalse SomeFalse where
	or m _ = m

instance Or AllFalse AllTrue AllTrue where
	or _ _ = AllTrue
instance Or AllFalse SomeTrue SomeTrue where
	or _ m = m
instance Or AllFalse SomeFalse SomeFalse where
	or _ m = m
instance Or AllFalse AllFalse AllFalse where
	or _ _ = AllFalse

------------------------------------------------------------------------------

class TypeEqWith x y k w where
	typeEqWith :: x -> y -> k -> w
instance Cont k AllTrue w => TypeEqWith x x k w where
	typeEqWith _ _ k = cont k AllTrue
instance Cont k AllFalse w => TypeEqWith x y k w where
	typeEqWith _ _ k = cont k AllFalse

------------------------------------------------------------------------------
