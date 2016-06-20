{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.TIR.Peano where

import Prelude hiding (Eq,div,mod,abs,sum,True,False)
import Numeric (showInt,showSigned)

import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved. 
------------------------------------------------------------------------------

data Zero = Zero 
data NaN = NaN
data Suc n = Suc n 
data Pre n = Pre n

class NotNegative m => ReifyIntegral m t i w where
    reifyIntegral :: Integral i => m -> t -> i -> w
instance (ReifyNotNegative m t i (Suc Zero) w,ReifyNotPositive m t i (Pre Zero) w,Ctrl.Apply t Zero w)
        => ReifyIntegral (Suc m) t i w where
    reifyIntegral (Suc m) t i
        | i>0 = reifyNotNegative m t (i-1) (Suc zero)
        | i<0 = reifyNotPositive m t (i+1) (Pre zero)
        | otherwise = Ctrl.apply t zero

class (NotNegative m,NotNegative n) => ReifyNotNegative m t i n w where
    reifyNotNegative :: Integral i => m -> t -> i -> n -> w
instance NotNegative n => ReifyNotNegative Zero t i n w where
    reifyNotNegative _ _ _ _ = error "Number to be reified exceeds maximum range."
instance (NotNegative n,ReifyNotNegative m t i (Suc n) w,Ctrl.Apply t n w)
    => ReifyNotNegative (Suc m) t i n w where
    reifyNotNegative (Suc m) t i n
        | i>0 = reifyNotNegative m t (i-1) (Suc n)
        | otherwise = Ctrl.apply t n

class (NotNegative m,NotPositive n) => ReifyNotPositive m t i n w where
    reifyNotPositive :: Integral i => m -> t -> i -> n -> w
instance NotPositive n => ReifyNotPositive Zero t i n w where
    reifyNotPositive _ _ _ _ = error "Number to be reified exceeds maximum range."
instance (NotPositive n,ReifyNotPositive m t i (Pre n) w,Ctrl.Apply t n w)
    => ReifyNotPositive (Suc m) t i n w where
    reifyNotPositive (Suc m) t i n
        | i<0 = reifyNotPositive m t (i+1) (Pre n)
        | otherwise = Ctrl.apply t n

class Show n => Number n where
    reflectNumber :: Integral a => n -> a
instance Number Zero where
    reflectNumber _ = 0
instance Number NaN where
    reflectNumber _ = undefined
instance Number n => Number (Suc n) where
    reflectNumber (Suc n) = reflectNumber n + 1
instance Number n => Number (Pre n) where
    reflectNumber (Pre n) = reflectNumber n - 1

class Number n => NotNegative n
instance NotNegative Zero
instance NotNegative NaN
instance NotNegative n => NotNegative (Suc n)

class Number n => NotPositive n
instance NotPositive Zero
instance NotPositive NaN
instance NotPositive n => NotPositive (Pre n)

class Number n => NotZero n
instance NotZero NaN
instance NotNegative n => NotZero (Suc n)
instance NotPositive n => NotZero (Pre n)

class NotNegative n => Positive n
instance Positive NaN
instance NotNegative n => Positive (Suc n)

class NotPositive n => Negative n
instance Negative NaN
instance NotPositive n => Negative (Pre n)

------------------------------------------------------------------------------

instance Show Zero where
    showsPrec _ _ = showInt (0 :: Integer)
instance Show NaN where
    showsPrec _ _ = showString "NaN"
instance Number n => Show (Suc n) where
    showsPrec _ n = showInt (reflectNumber n :: Integer)
instance Number n => Show (Pre n) where
    showsPrec p n = showSigned showInt p (reflectNumber n :: Integer)

------------------------------------------------------------------------------

class (Number n,Number n') => Negate n n' | n -> n', n' -> n where
    neg :: n -> n'
instance Negate Zero Zero where
    neg _ = zero
instance (Negate n n',NotNegative n,NotPositive n') => Negate (Suc n) (Pre n') where
    neg (Suc n) = Pre (neg n)
instance (Negate n n',NotPositive n,NotNegative n') => Negate (Pre n) (Suc n') where
    neg (Pre n) = Suc (neg n)

class (Number n,NotNegative n') => Abs n n' | n -> n' where
    abs :: n -> n'
instance Abs Zero Zero where
    abs _ = zero
instance NotNegative n => Abs (Suc n) (Suc n) where
    abs n = n
instance (NotPositive n,NotNegative n',Negate n n') => Abs (Pre n) (Suc n') where
    abs n = neg n

------------------------------------------------------------------------------

class (Number n,Number n') => Increment n n' | n -> n', n' -> n where
    inc :: n -> n'
instance Increment Zero (Suc Zero) where
    inc _ = one
instance Increment (Pre Zero) Zero where
    inc _ = zero
instance NotPositive n => Increment (Pre (Pre n)) (Pre n) where
    inc (Pre n) = n
instance NotNegative n => Increment (Suc n) (Suc (Suc n)) where
    inc n = Suc n

class (Number n,Number n') => Decrement n n' | n -> n', n' -> n where
    dec :: n -> n'
instance Decrement Zero (Pre Zero) where
    dec _ = neg one
instance Decrement (Suc Zero) Zero where
    dec _ = zero
instance NotPositive n => Decrement (Pre n) (Pre (Pre n)) where
    dec n = Pre n
instance NotNegative n => Decrement (Suc (Suc n)) (Suc n) where
    dec (Suc n) = n
    
------------------------------------------------------------------------------

class (Number n1,Number n2,Number n3) => Add' n1 n2 n3 | n1 n2 -> n3 where
    add' :: n1 -> n2 -> n3
instance Add' Zero Zero Zero where
    add' _ _ = zero
instance NotNegative n => Add' Zero (Suc n) (Suc n) where
    add' _ n = n
instance NotPositive n => Add' Zero (Pre n) (Pre n) where
    add' _ n = n
instance NotNegative n => Add' (Suc n) Zero (Suc n) where
    add' n _ = n
instance NotPositive n => Add' (Pre n) Zero (Pre n) where
    add' n _ = n
instance (Add' n1 n2 n3,NotNegative n1,NotNegative n2,NotNegative n3) => Add' (Suc n1) (Suc n2) (Suc (Suc n3)) where
    add' (Suc n1) (Suc n2) = (Suc . Suc) (add' n1 n2)
instance (Add' n1 n2 n3,NotPositive n1,NotPositive n2,NotPositive n3) => Add' (Pre n1) (Pre n2) (Pre (Pre n3)) where
    add' (Pre n1) (Pre n2) = (Pre . Pre) (add' n1 n2)
instance (Add' n1 n2 n3,NotNegative n1,NotPositive n2) => Add' (Suc n1) (Pre n2) n3 where
    add' (Suc n1) (Pre n2) = add' n1 n2
instance (Add' n1 n2 n3,NotPositive n1,NotNegative n2) => Add' (Pre n1) (Suc n2) n3 where
    add' (Pre n1) (Suc n2) = add' n1 n2

class (Number n1,Number n2,Number n3) => Sub' n1 n2 n3 | n1 n2 -> n3 where
    sub' :: n1 -> n2 -> n3
instance Sub' Zero Zero Zero where
    sub' _ _ = zero
instance (Negate n n',NotNegative n,NotPositive n') => Sub' Zero (Suc n) (Pre n') where
    sub' _ (Suc n) = Pre (neg n)
instance (Negate n n',NotPositive n,NotNegative n') => Sub' Zero (Pre n) (Suc n') where
    sub' _ (Pre n) = Suc (neg n)
instance NotNegative n => Sub' (Suc n) Zero (Suc n) where
    sub' n _ = n
instance NotPositive n => Sub' (Pre n) Zero (Pre n) where
    sub' n _ = n
instance (Sub' n1 n2 n3,NotNegative n1,NotPositive n2,NotNegative n3) => Sub' (Suc n1) (Pre n2) (Suc (Suc n3)) where
    sub' (Suc n1) (Pre n2) = (Suc . Suc) (sub' n1 n2)
instance (Sub' n1 n2 n3,NotPositive n1,NotNegative n2,NotPositive n3) => Sub' (Pre n1) (Suc n2) (Pre (Pre n3)) where
    sub' (Pre n1) (Suc n2) = (Pre . Pre) (sub' n1 n2)
instance (Sub' n1 n2 n3,NotNegative n1,NotNegative n2) => Sub' (Suc n1) (Suc n2) n3 where
    sub' (Suc n1) (Suc n2) = sub' n1 n2
instance (Sub' n1 n2 n3,NotPositive n1,NotPositive n2)  => Sub' (Pre n1) (Pre n2) n3 where
    sub' (Pre n1) (Pre n2) = sub' n1 n2

------------------------------------------------------------------------------

class (Add' x y z,Sub' z y x,Sub' z x y) => Add x y z | x y -> z, z x -> y, z y -> x where
    add :: x -> y -> z
instance (Add' x y z,Sub' z y x,Sub' z x y) => Add x y z where
    add = add'

class (Sub' x y z,Sub' x z y,Add' y z x) => Sub x y z | x y -> z, z x -> y, z y -> x where
    sub :: x -> y -> z
instance (Sub' x y z,Sub' x z y,Add' y z x) => Sub x y z where
    sub = sub'

------------------------------------------------------------------------------

class (Number n1,Number n2,Number n3) => Mul n1 n2 n3 | n1 n2 -> n3 where
    mul :: n1 -> n2 -> n3
instance Mul Zero Zero Zero where
    mul _ _ = zero
instance NotNegative n => Mul Zero (Suc n) Zero where
    mul _ _ = zero
instance NotPositive n => Mul Zero (Pre n) Zero where
    mul _ _ = zero
instance NotNegative n => Mul (Suc n) Zero Zero where
    mul _ _ = zero
instance NotPositive n => Mul (Pre n) Zero Zero where
    mul _ _ = zero
instance (NotNegative n1,Mul n1 (Suc n2) n3',Add n2 n3' n3) => Mul (Suc n1) (Suc n2) (Suc n3) where
    mul (Suc n1) (Suc n2) = Suc $ n2 `add` mul n1 (Suc n2)
instance (NotNegative n1,Mul n1 (Pre n2) n3',Add n2 n3' n3) => Mul (Suc n1) (Pre n2) (Pre n3) where
    mul (Suc n1) (Pre n2) = Pre $ n2 `add` mul n1 (Pre n2)
instance (NotPositive n1,Mul n1 (Suc n2) n3',Negate n2 m2,Add m2 n3' n3) => Mul (Pre n1) (Suc n2) (Pre n3) where
    mul (Pre n1) (Suc n2) = Pre $ neg n2 `add` mul n1 (Suc n2)
instance (NotPositive n1,Mul n1 (Pre n2) n3',Negate n2 m2,Add m2 n3' n3) => Mul (Pre n1) (Pre n2) (Suc n3) where
    mul (Pre n1) (Pre n2) = Suc $ neg n2 `add` mul n1 (Pre n2)

------------------------------------------------------------------------------

class (Number n,Number n') => Eq n n' t | n n' -> t where
    eq :: n -> n' -> t
instance Eq Zero Zero Logic.AllTrue where
    eq _ _ = Logic.AllTrue
instance NotNegative n => Eq (Suc n) Zero Logic.AllFalse where
    eq (Suc _) _ = Logic.AllFalse
instance NotPositive n => Eq (Pre n) Zero Logic.AllFalse where
    eq (Pre _) _ = Logic.AllFalse
instance NotNegative n => Eq Zero (Suc n) Logic.AllFalse where
    eq _ (Suc _) = Logic.AllFalse
instance NotPositive n => Eq Zero (Pre n) Logic.AllFalse where
    eq _ (Pre _) = Logic.AllFalse
instance (NotPositive n,NotNegative n') => Eq (Pre n) (Suc n') Logic.AllFalse where
    eq (Pre _) (Suc _) = Logic.AllFalse
instance (NotNegative n,NotPositive n') => Eq (Suc n) (Pre n') Logic.AllFalse where
    eq (Suc _) (Pre _) = Logic.AllFalse
instance (NotNegative n,NotNegative n',Eq n n' t) => Eq (Suc n) (Suc n') t where
    eq (Suc n) (Suc n') = eq n n'
instance (NotPositive n,NotPositive n',Eq n n' t) => Eq (Pre n) (Pre n') t where
    eq (Pre n) (Pre n') = eq n n'

class (Number n,Number n') => Ne n n' t | n n' -> t where
    ne :: n -> n' -> t
instance Ne Zero Zero Logic.AllFalse where
    ne _ _ = Logic.AllFalse
instance NotNegative n => Ne (Suc n) Zero Logic.AllTrue where
    ne (Suc _) _ = Logic.AllTrue
instance NotPositive n => Ne (Pre n) Zero Logic.AllTrue where
    ne (Pre _) _ = Logic.AllTrue
instance NotNegative n => Ne Zero (Suc n) Logic.AllTrue where
    ne _ (Suc _) = Logic.AllTrue
instance NotPositive n => Ne Zero (Pre n) Logic.AllTrue where
    ne _ (Pre _) = Logic.AllTrue
instance (NotPositive n,NotNegative n') => Ne (Pre n) (Suc n') Logic.AllTrue where
    ne (Pre _) (Suc _) = Logic.AllTrue
instance (NotNegative n,NotPositive n') => Ne (Suc n) (Pre n') Logic.AllTrue where
    ne (Suc _) (Pre _) = Logic.AllTrue
instance (NotNegative n,NotNegative n',Ne n n' t) => Ne (Suc n) (Suc n') t where
    ne (Suc n) (Suc n') = ne n n'
instance (NotPositive n,NotPositive n',Ne n n' t) => Ne (Pre n) (Pre n') t where
    ne (Pre n) (Pre n') = ne n n'

class (Number n,Number n') => Le n n' t | n n' -> t where
    le :: n -> n' -> t
instance Le Zero Zero Logic.AllTrue where
    le _ _ = Logic.AllTrue
instance NotNegative n => Le (Suc n) Zero Logic.AllFalse where
    le (Suc _) _ = Logic.AllFalse
instance NotPositive n => Le (Pre n) Zero Logic.AllTrue where
    le (Pre _) _ = Logic.AllTrue
instance NotNegative n => Le Zero (Suc n) Logic.AllTrue where
    le _ (Suc _) = Logic.AllTrue
instance NotPositive n => Le Zero (Pre n) Logic.AllFalse where
    le _ (Pre _) = Logic.AllFalse
instance (NotPositive n,NotNegative n') => Le (Pre n) (Suc n') Logic.AllTrue where
    le (Pre _) (Suc _) = Logic.AllTrue
instance (NotNegative n,NotPositive n') => Le (Suc n) (Pre n') Logic.AllFalse where
    le (Suc _) (Pre _) = Logic.AllFalse
instance (NotNegative n,NotNegative n',Le n n' t) => Le (Suc n) (Suc n') t where
    le (Suc n) (Suc n') = le n n'
instance (NotPositive n,NotPositive n',Le n n' t) => Le (Pre n) (Pre n') t where
    le (Pre n) (Pre n') = le n n'

class (Number n,Number n') => Ge n n' t | n n' -> t where
    ge :: n -> n' -> t
instance Ge  Zero Zero Logic.AllTrue where
    ge _ _ = Logic.AllTrue
instance NotNegative n => Ge (Suc n) Zero Logic.AllTrue where
    ge (Suc _) _ = Logic.AllTrue
instance NotPositive n => Ge (Pre n) Zero Logic.AllFalse where
    ge (Pre _) _ = Logic.AllFalse
instance NotNegative n => Ge Zero (Suc n) Logic.AllFalse where
    ge _ (Suc _) = Logic.AllFalse
instance NotPositive n => Ge Zero (Pre n) Logic.AllTrue where
    ge _ (Pre _) = Logic.AllTrue
instance (NotPositive n,NotNegative n') => Ge (Pre n) (Suc n') Logic.AllFalse where
    ge (Pre _) (Suc _) = Logic.AllFalse
instance (NotNegative n,NotPositive n') => Ge (Suc n) (Pre n') Logic.AllTrue where
    ge (Suc _) (Pre _) = Logic.AllTrue
instance (NotNegative n,NotNegative n',Ge n n' t) => Ge (Suc n) (Suc n') t where
    ge (Suc n) (Suc n') = ge n n'
instance (NotPositive n,NotPositive n',Ge n n' t) => Ge (Pre n) (Pre n') t where
    ge (Pre n) (Pre n') = ge n n'

class (Number n,Number n') => Lt n n' t | n n' -> t where
    lt :: n -> n' -> t
instance Lt  Zero Zero Logic.AllFalse where
    lt _ _ = Logic.AllFalse
instance NotNegative n => Lt (Suc n) Zero Logic.AllFalse where
    lt (Suc _) _ = Logic.AllFalse
instance NotPositive n => Lt (Pre n) Zero Logic.AllTrue where
    lt (Pre _) _ = Logic.AllTrue
instance NotNegative n => Lt Zero (Suc n) Logic.AllTrue where
    lt _ (Suc _) = Logic.AllTrue
instance NotPositive n => Lt Zero (Pre n) Logic.AllFalse where
    lt _ (Pre _) = Logic.AllFalse
instance (NotPositive n,NotNegative n') => Lt (Pre n) (Suc n') Logic.AllTrue where
    lt (Pre _) (Suc _) = Logic.AllTrue
instance (NotNegative n,NotPositive n') => Lt (Suc n) (Pre n') Logic.AllFalse where
    lt (Suc _) (Pre _) = Logic.AllFalse
instance (NotNegative n,NotNegative n',Lt n n' t) => Lt (Suc n) (Suc n') t where
    lt (Suc n) (Suc n') = lt n n'
instance (NotPositive n,NotPositive n',Lt n n' t) => Lt (Pre n) (Pre n') t where
    lt (Pre n) (Pre n') = lt n n'

class (Number n,Number n') => Gt n n' t | n n' -> t where
    gt :: n -> n' -> t
instance Gt  Zero Zero Logic.AllFalse where
    gt _ _ = Logic.AllFalse
instance NotNegative n => Gt (Suc n) Zero Logic.AllTrue where
    gt (Suc _) _ = Logic.AllTrue
instance NotPositive n => Gt (Pre n) Zero Logic.AllFalse where
    gt (Pre _) _ = Logic.AllFalse
instance NotNegative n => Gt Zero (Suc n) Logic.AllFalse where
    gt _ (Suc _) = Logic.AllFalse
instance NotPositive n => Gt Zero (Pre n) Logic.AllTrue where
    gt _ (Pre _) = Logic.AllTrue
instance (NotPositive n,NotNegative n') => Gt (Pre n) (Suc n') Logic.AllFalse where
    gt (Pre _) (Suc _) = Logic.AllFalse
instance (NotNegative n,NotPositive n') => Gt (Suc n) (Pre n') Logic.AllTrue where
    gt (Suc _) (Pre _) = Logic.AllTrue
instance (NotNegative n,NotNegative n',Gt n n' t) => Gt (Suc n) (Suc n') t where
    gt (Suc n) (Suc n') = gt n n'
instance (NotPositive n,NotPositive n',Gt n n' t) => Gt (Pre n) (Pre n') t where
    gt (Pre n) (Pre n') = gt n n'

------------------------------------------------------------------------------
-- Note: Hugs does not like constraining the RHS of a fundep, when an instance constraint
-- does not share the constraint. The problem here is Conditional, which is
-- not constrained in any way.

class (NotNegative n1,Positive n2) => UDiv n1 n2 n3 | n1 n2 -> n3 where
    udiv :: n1 -> n2 -> n3
instance NotNegative n => UDiv Zero (Suc n) Zero where
    udiv _ _ = zero
instance (NotNegative n1,NotNegative n2,
        Ge n1 n2 t,
        Logic.Conditional t UDivFn' (Ctrl.ConstFn Zero) f,
        Ctrl.Apply f (Suc n1,Suc n2) n3) => UDiv (Suc n1) (Suc n2) n3 where
    udiv (Suc n1) (Suc n2) = Ctrl.apply (Logic.cond (n1 `ge` n2) UDivFn' (Ctrl.ConstFn zero)) (Suc n1,Suc n2)

data UDivFn' = UDivFn'
instance (Sub x y s,UDiv s y u,Increment u z) => Ctrl.Apply UDivFn' (x,y) z where
    apply _ (x,y) = inc $ udiv (x `sub` y) y

------------------------------------------------------------------------------

class (Number n1,NotZero n2) => Div n1 n2 n3 | n1 n2 -> n3 where
    div :: n1 -> n2 -> n3
instance NotZero n => Div Zero n Zero where
    div _ _ = zero
instance (NotNegative x,NotNegative y,UDiv (Suc x) (Suc y) z) => Div (Suc x) (Suc y) z where
    div = udiv
instance (Negate (Pre x) (Suc x'),NotNegative x',NotNegative y,
        UDiv (Suc x') (Suc y) z,Negate z z') => Div (Pre x) (Suc y) z' where
    div x y = neg $ udiv (neg x) y
instance (NotPositive y,Negate (Pre y) (Suc y'),NotNegative x,NotNegative y',
        UDiv (Suc x) (Suc y') z,Negate z z') => Div (Suc x) (Pre y) z' where
    div x y = neg $ udiv x (neg y)
instance (NotPositive x,NotPositive y,Negate (Pre x) (Suc x'),
        Negate (Pre y) (Suc y'),NotNegative x',NotNegative y',
        UDiv (Suc x') (Suc y') z) => Div (Pre x) (Pre y) z where
    div x y = udiv (neg x) (neg y)

------------------------------------------------------------------------------

class (NotNegative n1,Positive n2) => UMod n1 n2 n3 | n1 n2 -> n3 where
    umod :: n1 -> n2 -> n3
instance NotNegative n => UMod Zero (Suc n) Zero where
    umod _ _ = zero
instance (NotNegative n1,NotNegative n2,
        Ge n1 n2 t,
        Logic.Conditional t UModFn' (Ctrl.ConstFn (Suc n1)) f,
        Ctrl.Apply f (Suc n1,Suc n2) n3) => UMod (Suc n1) (Suc n2) n3 where
    umod (Suc n1) (Suc n2) = Ctrl.apply (Logic.cond (n1 `ge` n2) UModFn' (Ctrl.ConstFn (Suc n1))) (Suc n1,Suc n2)

data UModFn' = UModFn'
instance (Sub x y s,UMod s y z) => Ctrl.Apply UModFn' (x,y) z where
    apply _ (x,y) = umod (x `sub` y) y

------------------------------------------------------------------------------

class (Number n1,NotZero n2) => Mod n1 n2 n3 | n1 n2 -> n3 where
    mod :: n1 -> n2 -> n3
instance NotZero n => Mod Zero n Zero where
    mod _ _ = zero
instance (NotNegative x,NotNegative y,UMod (Suc x) (Suc y) z) => Mod (Suc x) (Suc y) z where
    mod = umod
instance (Negate (Pre x) (Suc x'),NotNegative x',NotNegative y,
        UMod (Suc x') (Suc y) z,Negate z z') => Mod (Pre x) (Suc y) z' where
    mod x y = neg $ umod (neg x) y
instance (NotPositive y,Negate (Pre y) (Suc y'),NotNegative x,NotNegative y',
        UMod (Suc x) (Suc y') z) => Mod (Suc x) (Pre y) z where
    mod x y = umod x (neg y)
instance (NotPositive x,NotPositive y,Negate (Pre x) (Suc x'),
        Negate (Pre y) (Suc y'),NotNegative x',NotNegative y',
        UMod (Suc x') (Suc y') z,Negate z z') => Mod (Pre x) (Pre y) z' where
    mod x y = neg $ umod (neg x) (neg y)

------------------------------------------------------------------------------

class (Number n,Number v,Number w) => FoldN f v n w | f v n -> w where
    foldN :: f -> v -> n -> w
instance Number v => FoldN f v Zero v where
    foldN _ v _ = v
instance (NotNegative n,Number w',FoldN f v n w,Ctrl.Apply f (Logic.AllTrue,w) w') => FoldN f v (Suc n) w' where
    foldN f v (Suc n) = Ctrl.apply f (Logic.AllTrue,(foldN f v n))
instance (NotPositive n,Number w',FoldN f v n w,Ctrl.Apply f (Logic.AllFalse,w) w') => FoldN f v (Pre n) w' where
    foldN f v (Pre n) = Ctrl.apply f (Logic.AllFalse,(foldN f v n))

------------------------------------------------------------------------------

zero :: Zero
zero = Zero

type One = Suc Zero
one :: One
one = Suc zero

type Two = Suc One
two :: Two
two = Suc one

type Three = Suc Two
three :: Three
three = Suc two

type Four = Suc Three
four :: Four
four = Suc three

type Five = Suc Four
five :: Five
five = Suc four

type Six = Suc Five
six :: Six
six = Suc five

type Seven = Suc Six
seven :: Seven
seven = Suc six

type Eight = Suc Seven
eight :: Eight 
eight = Suc seven

type Nine = Suc Eight
nine :: Nine
nine = Suc eight

type Ten = Suc Nine
ten :: Ten
ten = Suc nine

