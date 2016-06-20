{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Lib.TIR.TIR where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, All Rights Reserved.
------------------------------------------------------------------------------

import Prelude hiding (map,not,null,and,zip,unzip,lookup,init,last,True,False,filter)

import qualified Data.Generics as Generics
import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic
import qualified Lib.TIR.Peano as Peano

------------------------------------------------------------------------------

-- instance Data ()

infixr 1 `Cons`
data Nil = Nil deriving (Generics.Data, Generics.Typeable)
--data TIR r => Cons a r = Cons a r deriving (Data,Typeable)
data Cons a r = Cons a r deriving (Generics.Data, Generics.Typeable)

infixr 2 :#:
type a :#: r = Cons a r

infixr 1 `cons`
cons :: TIR r => a -> r -> Cons a r
cons a r = Cons a r

infixr 1 .#.
(.#.) :: TIR r => a -> r -> Cons a r
a .#. r = Cons a r

------------------------------------------------------------------------------

class TIR r
instance TIR Nil 
instance TIR r => TIR (Cons a r)

class TIR r => ShowTIR r where
    showTIR' :: r -> ShowS
instance ShowTIR Nil where
    showTIR' _ = showChar '>'
instance Show a => ShowTIR (Cons a Nil) where
    showTIR' (Cons a _) = shows a . showChar '>'
instance (Show a,ShowTIR (Cons b r)) => ShowTIR (Cons a (Cons b r)) where
    showTIR' (Cons a r) = shows a . showChar ',' . showTIR' r

showTIR :: ShowTIR r => r -> ShowS
showTIR r = showChar '<' . showTIR' r

instance Show Nil where
    showsPrec _ r = showTIR r
instance (ShowTIR r,ShowTIR (Cons a r)) => Show (Cons a r) where
    showsPrec _ r = showTIR r

------------------------------------------------------------------------------

-- Constraint for TypeIndexedProduct 
class TIR r => TIP r
instance TIP Nil
instance (TIP r,Member r a Logic.False) => TIP (Cons a r)


-- Constraint for TypeIndexedCoProduct 
class (TIP t,Member t a Logic.True) => TIC t a
instance (TIP t,Member t a Logic.True) => TIC t a


class Partition p 
instance Partition Nil
instance (TIP a,Fold (DjFn a) Logic.True p Logic.True) => Partition (Cons a p)

data DjFn a = DjFn a
instance (Disjunct s a t,Logic.And v t w) => Ctrl.Apply (DjFn a) (s,v) w where
    apply (DjFn a) (s,v) = v `Logic.and` disjunct s a

------------------------------------------------------------------------------

empty :: Nil
empty = Nil

singleton :: a -> Cons a Nil
singleton a = Cons a empty

------------------------------------------------------------------------------

class (TIR r,Logic.TVL t) => Null r t | r -> t where
    null :: r -> t
instance Null Nil Logic.True where
    null _ = Logic.true
instance TIR r => Null (Cons a r) Logic.False where
    null _ = Logic.false

class (TIR r,Logic.TVL t) => Member r a t | r a -> t where
    member :: r -> a -> t
instance Member Nil a Logic.False where
    member _ _ = Logic.false
instance TIR r => Member (Cons a r) a Logic.True where
    member _ _ = Logic.true
instance Member r a t => Member (Cons b r) a t where
    member (Cons _ r) a = member r a

class (TIR r,TIR s) => Subset r s t | r s -> t where
    subset :: r -> s -> t
instance TIR r => Subset r Nil Logic.True where
    subset _ _ = Logic.true
instance (Subset r s t2,Member r a t3,Logic.And t2 t3 t) => Subset r (Cons a s) t where
    subset r (Cons a s) = subset r s `Logic.and` member r a

-- two sets are disjunct if r member no element-types from s
class (TIR r,TIR s,Logic.TVL t) => Disjunct r s t | r s -> t where
    disjunct :: r -> s -> t
instance TIR r => Disjunct r Nil Logic.True where
    disjunct _ _ = Logic.true
instance (Logic.TVL t,
        Disjunct r s t2,
        Member r a t3,
        Logic.Conditional t3 Logic.False t2 t) => Disjunct r (Cons a s) t where
    disjunct r (Cons a s) = Logic.cond (member r a) Logic.false (disjunct r s)

-- two sets are conjuct is r is a subset of s and s is a subset of r
class  (TIR r,TIR s) => Conjunct r s t | r s -> t where
    conjunct :: r -> s -> t
instance Conjunct Nil Nil Logic.False where
   conjunct _ _ = Logic.false
instance (Subset r s t1,Subset s r t2,Logic.And t1 t2 t3) =>  Conjunct r s t3 where
    conjunct r s = subset r s `Logic.and` subset s r

{- Requires 2 class trick -}
-- identical TIRs are conjunct and have the same items
-- in the same order. 
class (TIR r,TIR s,Logic.TVL t) => Identical r s t | r s -> t where
    identical :: r -> s -> t
instance Identical Nil Nil Logic.True where
    identical _ _ = Logic.true
instance TIR r => Identical (Cons a r) Nil Logic.False where
    identical _ _ = Logic.false
instance TIR r => Identical Nil (Cons a r) Logic.False where
    identical _ _ = Logic.false
instance Identical r s t => Identical (Cons a r) (Cons a s) t where
    identical (Cons _ r) (Cons _ s) = identical r s
instance Identical' r s t => Identical r s t where
    identical r s = identical' r s
class (TIR r,TIR s,Logic.TVL t) => Identical' r s t | r s -> t where
    identical' :: r -> s -> t
instance (TIR r,TIR s) => Identical' (Cons a r) (Cons b s) Logic.False where
    identical' _ _ = Logic.false

------------------------------------------------------------------------------

class (TIR r,Peano.NotNegative n) => Size r n | r -> n where
    size :: r -> n
instance Size Nil Peano.Zero where
    size _ = Peano.zero
instance Size r n => Size (Cons a r) (Peano.Suc n) where
    size (Cons _ r) = Peano.Suc (size r)

class (TIR r,Peano.NotNegative n) => Count r a n | r a -> n where
    count :: r -> a -> n
instance Count Nil a Peano.Zero where
    count _ _ = Peano.zero
instance Count r a n => Count (Cons a r) a (Peano.Suc n) where
    count (Cons _ r) a = Peano.Suc (count r a)
instance Count r a n => Count (Cons b r) a n where
    count (Cons _ r) a = count r a

------------------------------------------------------------------------------

data TIR_Empty = TIR_Empty

class TIR r => Head r a | r -> a where
    head :: r -> a
instance Ctrl.AssertFail TIR_Empty => Head Nil () where
    head _ = ()
instance TIR r => Head (Cons a r) a where
    head (Cons a _) = a

class (TIR r,TIR r') => Tail r r' | r -> r' where
    tail :: r -> r'
instance Ctrl.AssertFail TIR_Empty => Tail Nil Nil where
    tail _ = empty
instance TIR r => Tail (Cons a r) r where
    tail (Cons _ r) = r

------------------------------------------------------------------------------

class TIR r => Last r a | r -> a where
    last :: r -> a
instance Ctrl.AssertFail TIR_Empty => Last Nil () where
    last _ = ()
instance Last (Cons a Nil) a where
    last (Cons a _) = a
instance Last r a => Last (Cons b r) a where
    last (Cons _ r) = last r

class (TIR r,TIR r') => Init r r' | r -> r' where
    init :: r -> r'
instance Ctrl.AssertFail TIR_Empty => Init Nil Nil where
    init _ = empty
instance Init (Cons a Nil) Nil where
    init (Cons _ _) = empty
instance Init (Cons b r) r' => Init (Cons a (Cons b r)) (Cons a r') where
    init (Cons a r) = Cons a (init r)

------------------------------------------------------------------------------

class (TIR r,TIR r') => Prepend r a r' | r a -> r', r' a -> r, r r' -> a where
    prepend :: r -> a -> r'
instance TIR r => Prepend r a (Cons a r) where
    prepend r a = Cons a r

class (TIR r,TIR r') => Append r a r' | r a -> r', r r' -> a where
    append :: r -> a -> r'
instance Append Nil a (Cons a Nil) where
    append _ a = Cons a empty
instance Append  r a r' => Append (Cons a' r) a (Cons a' r') where
    append (Cons a r) a' = Cons a (append r a')

------------------------------------------------------------------------------

data IndexTooLarge = IndexTooLarge

-- If instance for Nil included, cannot detect project failure at compile time.
class (Peano.NotNegative n,TIR r) => Index r n a | r n -> a where
    index :: r -> n -> a
instance (Ctrl.AssertFail IndexTooLarge,Peano.NotNegative n) => Index Nil n () where
    index _ _ = ()
instance TIR r => Index (Cons a r) Peano.Zero a where
    index (Cons a _) _ = a
instance Index r n a => Index (Cons b r) (Peano.Suc n) a where
    index (Cons _ r) (Peano.Suc n) = index r n

class (TIR r,Integral i) => Index' r i a where
    index' :: r -> i -> Maybe a
instance Integral i => Index' Nil i a where
    index' _ _ = Nothing
instance Index' r i a => Index' (Cons a r) i a where
    index' (Cons a _) 0 = Just a
    index' (Cons _ r) n = index' r (n-1)
instance Index' r i a => Index' (Cons b r) i a where
    index' (Cons _ _) 0 = Nothing
    index' (Cons _ r) n = index' r (n-1)

class (Peano.NotNegative n,TIR r,TIR r') => ReplaceIndex r n a r' | r n a -> r' where
    replaceIndex :: r -> n -> a -> r'
instance (Ctrl.AssertFail IndexTooLarge,Peano.NotNegative n) => ReplaceIndex Nil n a Nil where
    replaceIndex _ _ _ = empty
instance TIR r => ReplaceIndex (Cons b r) Peano.Zero a (Cons a r) where
    replaceIndex (Cons _ r) _ a = Cons a r
instance ReplaceIndex r n a r' => ReplaceIndex (Cons b r) (Peano.Suc n) a (Cons b r') where
    replaceIndex (Cons b r) (Peano.Suc n) a = Cons b (replaceIndex r n a)

------------------------------------------------------------------------------

-- Apply a polymorphic map for all types
class (TIR r,TIR r') => Map f r r' | f r -> r' where
    map :: f -> r -> r'
instance Map f Nil Nil where
    map _ _ = empty
instance (Ctrl.Apply f a b,Map f r r') => Map f (Cons a r) (Cons b r') where
    map f (Cons a r) = Cons (Ctrl.apply f a) (map f r)

{- Requires 2 class trick -}
-- Apply map function to matching type only
class (TIR r,TIR r') => Map1 r a b r' | r a b -> r' where
    map1 :: (a -> b) -> r -> r'
instance Map1 Nil a b Nil where
    map1 _ _ = empty
instance Map1 r a b r' => Map1 (Cons a r) a b (Cons b r') where
    map1 f (Cons a r) = Cons (f a) (map1 f r)
instance Map1' r a b r' => Map1 r a b r' where
    map1 f r = map1' f r
class (TIR r,TIR r') => Map1' r a b r' | r a b -> r' where
    map1' :: (a -> b) -> r -> r'
instance Map1 r a b r' => Map1' (Cons c r) a b (Cons c r') where
    map1' f (Cons c r) = Cons c (map1 f r)

class (TIR r,TIR r') => CMap c r r' | c r -> r', c r' -> r where
    cmap :: (forall a . a -> c a) -> r -> r'
instance CMap c Nil Nil where
    cmap _ _ = empty
instance CMap c r r' => CMap c (Cons a r) (Cons (c a) r') where
    cmap c (Cons a r) = Cons (c a) (cmap c r)

------------------------------------------------------------------------------

-- A polymorphic fold for all types
class TIR r => Fold f v r w | f v r -> w where
    fold :: f -> v -> r -> w
instance Fold  f v Nil v where
    fold _ v _ = v
instance (Fold f v r w,Ctrl.Apply f (a,w) w') => Fold f v (Cons a r) w' where
    fold f v (Cons a r) = Ctrl.apply f (a,fold f v r)

-- Apply fold function to matching type only
class TIR r => Fold1 r a where
    fold1 :: (a -> b -> b) -> b -> r -> b
instance Fold1 Nil a where
    fold1 _ v _ = v
instance Fold1 r a => Fold1 (Cons b r) a where
    fold1 f v (Cons _ r) = fold1 f v r
instance Fold1 r a => Fold1 (Cons a r) a where
    fold1 f v (Cons a r) = f a (fold1 f v r)

------------------------------------------------------------------------------

class (TIR r1,TIR r2,TIR r3) => Zip r1 r2 r3 | r1 r2 -> r3, r1 r3 -> r2, r2 r3 -> r1 where
    zip :: r1 -> r2 -> r3
instance Zip Nil Nil Nil where
    zip _ _ = empty
instance Zip r1 r2 r3 => Zip (Cons a r1) (Cons b r2) (Cons (a,b) r3) where
    zip (Cons a r) (Cons b s) = Cons (a,b) (zip r s)

class (TIR r1,TIR r2,TIR r3) => Unzip r1 r2 r3 | r1 -> r2 r3, r1 r3 -> r2, r2 r3 -> r1 where
    unzip :: r1 -> (r2,r3)
instance Unzip Nil Nil Nil where
    unzip _ = (empty,empty)
instance Unzip r1 r2 r3 => Unzip (Cons (a,b) r1) (Cons a r2) (Cons b r3) where
    unzip (Cons (a,b) r1) = (Cons a r2,Cons b r3) where
        (r2,r3) = unzip r1

------------------------------------------------------------------------------

class (TIR r,TIR r') => Select r a r' | r a -> r' where
   select :: r -> a -> r'
instance Select Nil a Nil where
    select _ _ = empty
instance Select r a r' => Select (Cons a r) a (Cons a r') where
   select (Cons b r) a = Cons b (select r a)
instance Select r a r' => Select (Cons b r) a r' where
   select (Cons _ r) a = select r a

{- Requires 2 class trick -}
class (TIR r,TIR r') => Except r a r' | r a -> r' where
    except :: r -> a -> r'
instance Except Nil a Nil where
    except _ _ = empty
instance Except r a r' => Except (Cons a r) a r' where
    except (Cons _ r) a = except r a
instance Except' r a r' => Except r a r' where
    except r a = except' r a
class (TIR r,TIR r') => Except' r a r' | r a -> r' where
    except' :: r -> a -> r'
instance Except r a r' => Except' (Cons b r) a (Cons b r') where
    except' (Cons b r) a = Cons b (except r a)

------------------------------------------------------------------------------

data TypeNotPresent = TypeNotPresent

class TIR r => Project r a where
    project :: r -> a
instance Ctrl.AssertFail TypeNotPresent => Project Nil () where
    project _ = ()
instance TIR r => Project (Cons a r) a where
    project (Cons a _) = a
instance Project r a => Project (Cons b r) a where
    project (Cons _ r) = project r

class TIR r => Inject r a where
    inject :: r -> a -> r
instance Ctrl.AssertFail TypeNotPresent => Inject Nil a where
    inject _ _ = empty
instance TIR r => Inject (Cons a r) a where
    inject (Cons _ r) a = Cons a r
instance Inject r a => Inject (Cons b r) a where
    inject (Cons b r) a = Cons b (inject r a)

------------------------------------------------------------------------------

class (TIR r,TIR r') => Insert r a r' | r a -> r', r r' -> a where
    insert :: r -> a -> r'
instance Insert Nil a (Cons a Nil) where
    insert r a = Cons a r
instance TIR r => Insert (Cons a r) a (Cons a r) where
    insert (Cons _ r) a = Cons a r
instance Insert r a r' => Insert (Cons b r) a (Cons b r') where
    insert (Cons b r) a = Cons b (insert r a)

{- Requires 2 class trick -}
class (TIR r,TIR r') => Replace r a b r' | r a b -> r' where
    replace :: r -> a -> b -> r'
instance Ctrl.AssertFail TypeNotPresent => Replace Nil a b Nil where
    replace _ _ _ = empty
instance TIR r => Replace (Cons a r) a b (Cons b r) where
    replace (Cons _ r) _ b = Cons b r
instance Replace' r a b r' => Replace r a b r' where
    replace r a b = replace' r a b
class (TIR r,TIR r') => Replace' r a b r' | r a b -> r' where
    replace' :: r -> a -> b -> r'
instance Replace r a b r' => Replace' (Cons c r) a b (Cons c r') where
    replace' (Cons c r) a b = Cons c (replace r a b)

data TypeNotPresentOrIndexTooLarge = TypeNotPresentOrIndexTooLarge

class (TIR r,Peano.NotNegative n) => Lookup r n a where
    lookup :: r -> n -> a
instance (Ctrl.AssertFail TypeNotPresentOrIndexTooLarge,Peano.NotNegative n) => Lookup Nil n () where
    lookup _ _ = ()
instance TIR r => Lookup (Cons a r) Peano.Zero a where
    lookup (Cons a _) _ = a
instance Lookup r n a => Lookup (Cons a r) (Peano.Suc n) a where
    lookup (Cons _ r) (Peano.Suc n) = lookup r n
instance Lookup r n a => Lookup (Cons b r) n a where
    lookup (Cons _ r) n = lookup r n

class (TIR r,Integral i) => Lookup' r i a where
    lookup' :: r -> i -> Maybe a
instance Integral i => Lookup' Nil i a where
    lookup' _ _ = Nothing
instance Lookup' r i a => Lookup' (Cons a r) i a where
    lookup' (Cons a _) 0 = Just a
    lookup' (Cons _ r) i = lookup' r (i-1)
instance Lookup' r i a => Lookup' (Cons b r) i a where
    lookup' (Cons _ _) 0 = Nothing
    lookup' (Cons _ r) i = lookup' r (i-1)

------------------------------------------------------------------------------

class (TIR r,TIR r') => UniqueRM r r' | r -> r' where
    unique :: r -> r'
instance UniqueRM Nil Nil where
    unique _ = empty
instance (Except r a r2,UniqueRM r2 r') => UniqueRM (Cons a r) (Cons a r') where
    unique (Cons a r) = Cons a (unique (except r a))

------------------------------------------------------------------------------

class (TIR r1,TIR r2,TIR r3) => Union r1 r2 r3 | r1 r2 -> r3, r1 r3 -> r2 where
    union :: r1 -> r2 -> r3
instance Union Nil Nil Nil where
    union _ _ = empty
instance TIR r => Union Nil (Cons a r) (Cons a r) where
    union _ r = r
instance Union r1 r2 r3 => Union (Cons a r1) r2 (Cons a r3) where
    union (Cons a r1) r2 = Cons a (r1 `union`  r2)

{- Requires 2 class trick -}
class (TIR r1,TIR r2,TIR r3) => Difference r1 r2 r3 | r1 r2 -> r3 where
    difference :: r1 -> r2 -> r3
instance Difference Nil Nil Nil where
    difference _ _ = empty
instance TIR r => Difference r Nil r where
    difference r _ = r
instance (Except r1 a r1Ea,
        Except r2 a r2Ea,
        Difference r1Ea r2Ea r1Dr2) => Difference r1 (Cons a r2) r1Dr2 where
    difference r1 (Cons a r2) = except r1 a `difference` except r2 a

{- Requires 2 class trick -}
class (TIR r1,TIR r2,TIR r3) => SymDifference r1 r2 r3 | r1 r2 -> r3 where
    symdifference :: r1 -> r2 -> r3
instance SymDifference Nil Nil Nil where
    symdifference _ _ = empty
instance TIR r => SymDifference Nil (Cons a r) (Cons a r) where
    symdifference _ r = r
instance TIR r => SymDifference (Cons a r) Nil (Cons a r) where
    symdifference r _ = r
instance (Select r1 a r1Sa,
        Select r2 a r2Sa,
        Null r1Sa t,
        Logic.Conditional t (Cons a r2Sa) Nil r1Cr2,
        Except r1 a r1Ea,
        Except r2 a r2Ea,
        SymDifference r1Ea r2Ea r1Dr2,
        Union r1Cr2 r1Dr2 r3) => SymDifference r1 (Cons a r2) r3 where
    symdifference r1 (Cons a r2) = (\r1Sa r2Sa -> Logic.cond (null r1Sa) (Cons a r2Sa) empty
        `union` (except r1 a `symdifference` except r2 a)) (select r1 a) (select r2 a)
    
{- Requires 2 class trick -}
class (TIR r1,TIR r2,TIR r3) => LeftIntersection r1 r2 r3 | r1 r2 -> r3 where
    leftIntersection :: r1 -> r2 -> r3
instance LeftIntersection Nil Nil Nil where
    leftIntersection _ _ = empty
instance TIR r => LeftIntersection Nil (Cons a r) Nil where
    leftIntersection _ _ = empty
instance TIR r => LeftIntersection (Cons a r) Nil Nil where
    leftIntersection _ _ = empty
instance (Select r1 a r1Sa,
        Except r1 a r1Ea,
        Except r2 a r2Ea,
        LeftIntersection r1Ea r2Ea r1Sr2',
        Union r1Sa r1Sr2' r1Sr2) => LeftIntersection r1 (Cons a r2) r1Sr2 where
    leftIntersection r1 (Cons a r2) = select r1 a `union` (except r1 a `leftIntersection` except r2 a)

{- Requires 2 class trick -}
class (TIR r1,TIR r2,TIR r3) => Intersection r1 r2 r3 | r1 r2 -> r3 where
    intersection :: r1 -> r2 -> r3
instance Intersection Nil Nil Nil where
    intersection _ _ = empty
instance TIR r => Intersection Nil (Cons a r) Nil where
    intersection _ _ = empty
instance TIR r => Intersection (Cons a r) Nil Nil where
    intersection _ _ = empty
instance (Select r1 a r1Sa,
        Select r2 a r2Sa,
        Null r1Sa t,
        Union r1Sa (Cons a r2Sa) r1Sr2,
        Logic.Conditional t Nil r1Sr2 r1Cr2,
        Except r1 a r1Ea,
        Except r2 a r2Ea,
        Intersection r1Ea r2Ea r1Ir2',
        Union r1Cr2 r1Ir2' r3) => Intersection r1 (Cons a r2) r3 where
    intersection r1 (Cons a r2) = (\r1Sa -> Logic.cond (null r1Sa) empty (r1Sa `union` Cons a (select r2 a))
        `union` (except r1 a `intersection` except r2 a)) (select r1 a)

------------------------------------------------------------------------------

class TIR r => Tuple t r | t -> r , r -> t where
    fromTuple :: t -> r
    toTuple :: r -> t

instance Tuple (a,b) (Cons a (Cons b Nil)) where
    fromTuple (a,b) = Cons a (Cons b empty)
    toTuple (Cons a (Cons b _)) = (a,b)

instance Tuple (a,b,c) (Cons a (Cons b (Cons c Nil))) where
    fromTuple (a,b,c) = Cons a (Cons b (Cons c empty))
    toTuple (Cons a (Cons b (Cons c _))) = (a,b,c)

{-

instance Tuple (a,b,c,d) (Cons a (Cons b (Cons c (Cons d Nil)))) where
    fromTuple (a,b,c,d) = Cons (a,Cons (b,Cons (c,Cons (d,empty))))
    toTuple (Cons (a,Cons (b,Cons (c,Cons (d,_))))) = (a,b,c,d)

instance Tuple (a,b,c,d,e) (Cons a (Cons b (Cons c (Cons d (Cons e Nil))))) where
    fromTuple (a,b,c,d,e) = Cons (a,Cons (b,Cons (c,Cons (d,Cons (e,empty)))))
    toTuple (Cons (a,Cons (b,Cons (c,Cons (d,Cons (e,_)))))) = (a,b,c,d,e)

instance Tuple (a,b,c,d,e,f) (Cons a (Cons b (Cons c (Cons d (Cons e (Cons f Nil)))))) where
    fromTuple (a,b,c,d,e,f) = Cons (a,Cons (b,Cons (c,Cons (d,Cons (e,Cons (f,empty)))))) 
    toTuple (Cons (a,Cons (b,Cons (c,Cons (d,Cons (e,Cons (f,_))))))) = (a,b,c,d,e,f)

-}

------------------------------------------------------------------------------

