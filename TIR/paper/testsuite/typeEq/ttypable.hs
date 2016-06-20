{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

module Foo where

-- very boring, but works everywhere...
class TypeEq x y
instance TypeEq Bool Bool
instance TypeEq Int Int
instance (TypeEq x x', TypeEq y y') => TypeEq (x->y) (x'->y')

class TypeNotEq x y
instance TypeNotEq Bool Int
instance TypeNotEq Int Bool
instance TypeNotEq (x->y) Bool

-- more advanced...
data HZero = HZero deriving Show
data HSucc a = HSucc a deriving Show

data HNil = HNil deriving Show
data HCons a b = HCons a b deriving Show

class TTypeable a b | a-> b
instance TTypeable Bool (HCons HZero HNil)
instance TTypeable Int  (HCons (HSucc HZero) HNil)
instance (TTypeable a al, TTypeable b bl) => 
         TTypeable (a->b)
	           (HCons (HSucc (HSucc HZero)) (HCons al (HCons bl HNil)))
instance (TTypeable a al) => 
         TTypeable [a]
	           (HCons (HSucc (HSucc (HSucc HZero))) (HCons al HNil))
instance (TTypeable a al) => 
         TTypeable (Maybe a)
	           (HCons (HSucc (HSucc (HSucc (HSucc HZero))))
 		          (HCons al HNil))
-- more instances for more type constructors

-- example of a higher-kind type
data Fix f = Fix (f (Fix f))
instance (TTypeable (f Bool) (HCons al l')) => 
         TTypeable (Fix f)
	           (HCons (HSucc (HSucc (HSucc (HSucc (HSucc HZero)))))
 		          (HCons al HNil))
		   		   

class TypeNotEq' x y -- generic
instance TypeNotEq' HNil (HCons a b)
instance TypeNotEq' (HCons a b) HNil 
instance TypeNotEq' (HCons HZero l) (HCons (HSucc a) l')
instance TypeNotEq' (HCons (HSucc a) l') (HCons HZero l)
instance TypeNotEq' (HCons a l) (HCons a' l') =>
         TypeNotEq' (HCons (HSucc a) l) (HCons (HSucc a') l')
instance TypeNotEq' l l' => TypeNotEq' (HCons HZero l) (HCons HZero l')
instance TypeNotEq' (HCons a (HCons t l)) (HCons a' (HCons t' l')) => 
         TypeNotEq' (HCons (HCons a t) l) (HCons (HCons a' t') l')
instance TypeNotEq' l l' => TypeNotEq' (HCons HNil l) (HCons HNil l')



class TypeNotEq'' x y
instance (TTypeable x a, TTypeable y b, TypeNotEq' a b) =>
         TypeNotEq'' x y

neq:: (TypeNotEq'' a b) => a -> b -> ()
neq _ _ = ()

data HTrue = HTrue
data HFalse = HFalse
instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"


class HTypeEqBool' x y r | x y -> r
instance HTypeEqBool' HNil HNil        HTrue
instance HTypeEqBool' HNil (HCons a b) HFalse
instance HTypeEqBool' (HCons a b) HNil HFalse
instance HTypeEqBool' l l' r =>
         HTypeEqBool' (HCons HZero l) (HCons HZero l') r
instance HTypeEqBool' (HCons HZero l) (HCons (HSucc a) l') HFalse
instance HTypeEqBool' (HCons (HSucc a) l') (HCons HZero l) HFalse
instance HTypeEqBool' (HCons n l) (HCons n' l') r =>
         HTypeEqBool' (HCons (HSucc n) l) (HCons (HSucc n') l') r
instance HTypeEqBool' l l' r =>
         HTypeEqBool' (HCons HNil l) (HCons HNil l') r
instance HTypeEqBool' (HCons HNil l) (HCons (HCons a' t') l') HFalse
instance HTypeEqBool' (HCons (HCons a' t') l') (HCons HNil l) HFalse
instance HTypeEqBool' (HCons a (HCons t l)) (HCons a' (HCons t' l')) r =>
         HTypeEqBool' (HCons (HCons a t) l) (HCons (HCons a' t') l') r

class HTypeEqBool x y r | x y -> r
instance (TTypeable x a, TTypeable y b, HTypeEqBool' a b r) =>
         HTypeEqBool x y r

teq:: (HTypeEqBool x y r) => x -> y -> r
teq = undefined

test = [
        show$ teq not (&&),
	show$ teq not not,
	show$ teq (&&) (||),
	show$ teq ((+)::(Int->Int->Int)) ((-)::(Int->Int->Int)),
	show$ teq ((*)::(Int->Int->Int)) ((*)::(Int->Int->Int)),
	show$ teq ((*)::(Int->Int->Int)) not,
	show$ teq True False,
	show$ teq (1::Int) True,
	show$ teq False ((+)::(Int->Int->Int)),
	show$ teq (||) ((+)::(Int->Int->Int)),
	show$ teq (undefined::Fix Maybe) True,
	show$ teq (undefined::Fix Maybe) (undefined::Fix []),
	show$ teq (undefined::Fix Maybe) (undefined::Fix Maybe)
	]


{-
class HCast r a b where
    hcast:: r -> a -> Maybe b
    
instance HCast HTrue  a a where hcast _ = Just
instance HCast HFalse a b where hcast _ = const Nothing

class Cast a b where cast :: a -> Maybe b
instance (TTypeable a x, TTypeable b y, HTypeEqBool' x y r, HCast r a b) 
         => Cast a b
	 where cast a :: Maybe b = hcast (teq a (undefined::b)) a

data WT x = forall a. TTypeable a x => WT a

data WR r = forall a b. HTypeEqBool' a b r => WR (WT a,WT b)

data WWT b r = forall a. HCast r a b => WWT a

data WTR x r b = forall a. (TTypeable a x,HCast r a b) => WTR a
data WTRR = WTRR (forall a x r b. (TTypeable a x,HCast r a b) => WTR x r b)

tcar::f a -> a; tcar = undefined

--uncast:: WTR x b -> Maybe b
uncast wta :: Maybe b =let wb = WT (undefined::b)
                           wa = case wta of WTRR (WTR a) -> WT a
                           wr = WR (wa,wb)

		       in case wta of WTRR (WTR a) -> hcast (tcar wr) a
-}
{-
uncast::(TTypeable b y,HTypeEqBool' x y r) => z -> (forall a. (TTypeable a x, HCast r a b) => a -> b) -> b
uncast z f = f z
-}		       			   
{-
data W = forall a. W (forall b x y r. (TTypeable a x, TTypeable b y, HTypeEqBool' x y r, HCast r a b) => (a, Maybe b))
	   
mkCast :: forall x. x -> W
mkCast x = W (x,cast x)
unCast :: W -> Maybe Bool
unCast (W z) = snd z

-- cast 'a' :: Maybe Int
-- cast 'a' :: Maybe Char
-}
