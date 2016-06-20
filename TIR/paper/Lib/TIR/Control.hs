{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.TIR.Control where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, All Rights Reserved.
------------------------------------------------------------------------------

class Apply t p a | t p -> a where
    apply :: t -> p -> a

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

instance (Monad m,Apply f x (m a)) => Apply (Sequence f) (x,m a) (m a) where
    apply (Sequence f) (x,c) = do { _ <- apply f x; c }

------------------------------------------------------------------------------

class TypeCast x y where
    typeCast :: x -> Maybe y
instance TypeCast x x where
    typeCast = Just
instance TypeCast x y where
    typeCast _ = Nothing

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
