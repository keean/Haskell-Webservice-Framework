{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.TIR.HArray where

------------------------------------------------------------------------------
-- (c) 2004 Keean Schupke, Ralf Laemmel & Oleg Kiselyov. All Rights Reserved.
------------------------------------------------------------------------------

import qualified Lib.TIR.Control as Ctrl
import qualified Lib.TIR.Logic as Logic
import qualified Lib.TIR.Peano as Peano
import Lib.TIR.HList

------------------------------------------------------------------------------
-- Type for a list of indexes

class HListIx l
instance HListIx HNil
instance (HListIx l,Peano.NotNegative n) => HListIx (HCons n l)

------------------------------------------------------------------------------
-- Array functions

data IndexTooLarge = IndexTooLarge

class (Peano.NotNegative n,HList r) => HLookup r n a | r n -> a where
    hLookup :: r -> n -> a
instance (Ctrl.AssertFail IndexTooLarge,Peano.NotNegative n,Ctrl.Force () v)
        => HLookup HNil (Peano.Suc n) v where
    hLookup _ _ = undefined
instance HList r => HLookup (HCons a r) Peano.Zero a where
    hLookup (HCons a _) _ = a
instance HLookup r n a => HLookup (HCons b r) (Peano.Suc n) a where
    hLookup (HCons _ r) (_ :: Peano.Suc n) = hLookup r (undefined :: n)

class (Peano.NotNegative n,HList l) => HLookupMaybe l n a | l n -> a where
    hLookupMaybe :: l -> n -> a
instance Peano.NotNegative n => HLookupMaybe HNil (Peano.Suc n) Ctrl.HNothing where
    hLookupMaybe _ _ = Ctrl.HNothing
instance HList l => HLookupMaybe (HCons a l) Peano.Zero (Ctrl.HJust a) where
    hLookupMaybe (HCons a _) _ = Ctrl.HJust a
instance HLookupMaybe l n a => HLookupMaybe (HCons b l) (Peano.Suc n) a where
    hLookupMaybe (HCons _ l) (_ :: Peano.Suc n) = hLookupMaybe l (undefined :: n)

class (Peano.NotNegative n,HList r) => HDelete r n r' | r n -> r' where
    hDelete :: r -> n -> r'
instance (Ctrl.AssertFail IndexTooLarge,Peano.NotNegative n)
         => HDelete HNil (Peano.Suc n) IndexTooLarge where
    hDelete _ _ = IndexTooLarge
instance HList r => HDelete (HCons b r) (Peano.Zero) r where
    hDelete (HCons _ r) _ = r
instance HDelete r n r' => HDelete (HCons b r) (Peano.Suc n) (HCons b r') where
    hDelete (HCons b r) (_ :: Peano.Suc n) = HCons b $ hDelete r (undefined :: n)

class (Peano.NotNegative n,HList r) => HUpdateTP r n a | r n -> a where
    hUpdateTP :: r -> n -> a -> r
instance (Ctrl.AssertFail IndexTooLarge,Peano.NotNegative n) => HUpdateTP HNil n IndexTooLarge where
    hUpdateTP  _ _ _ = HNil
instance HList r => HUpdateTP (HCons a r) Peano.Zero a where
    hUpdateTP (HCons _ r) _ a = HCons a r
instance HUpdateTP r n a => HUpdateTP (HCons a r) (Peano.Suc n) a where
    hUpdateTP (HCons b r) (_ :: Peano.Suc n) a = HCons b (hUpdateTP r (undefined :: n) a)

class (Peano.NotNegative n,HList r) => HUpdateTC r n a r' | r n a -> r' where
    hUpdateTC :: r -> n -> a -> r'
instance (Ctrl.AssertFail IndexTooLarge,Peano.NotNegative n) => HUpdateTC HNil n a IndexTooLarge where
    hUpdateTC  _ _ _ = IndexTooLarge
instance HList r => HUpdateTC (HCons b r) Peano.Zero a (HCons a r) where
    hUpdateTC (HCons _ r) _ a = HCons a r
instance HUpdateTC r n a s => HUpdateTC (HCons b r) (Peano.Suc n) a (HCons b s) where
    hUpdateTC (HCons b r) (_ :: Peano.Suc n) a = HCons b (hUpdateTC r (undefined :: n) a)

class (Peano.NotNegative i,HList l) => HMark c l i m | c l i -> m where
    hMark :: (forall a . a -> c a) -> l -> i -> m
instance Peano.NotNegative i => HMark c HNil i HNil where
    hMark _ _ _ = HNil
instance HList l => HMark c (HCons e l) Peano.Zero (HCons (c e) l) where
    hMark c (HCons e l) _ = HCons (c e) l
instance HMark c l i m => HMark c (HCons e l) (Peano.Suc i) (HCons e m) where
    hMark c (HCons e l) (_ :: Peano.Suc i) = HCons e (hMark c l (undefined :: i))

class (Peano.NotNegative i,HList l) => HUnmark c l i m | c l i -> m where
    hUnmark :: (forall a . c a -> a) -> l -> i -> m
instance Peano.NotNegative i => HUnmark c HNil i HNil where
    hUnmark _ _ _ = HNil
instance HList l => HUnmark c (HCons (c e) l) (Peano.Zero) (HCons e l) where
    hUnmark c (HCons e l) _ = HCons (c e) l
instance HUnmark c l i m => HUnmark c (HCons e l) (Peano.Suc i) (HCons e m) where
    hUnmark c (HCons e l) (_ :: Peano.Suc i) = HCons e (hUnmark c l (undefined :: i))

class (HListIx il,HList l) => HSplit l il l' l'' | l il -> l' l'' where
    hSplit :: l -> il -> (l',l'')
instance (HMarkAll Ctrl.HLeft l m,HSplit' m il l' l'') => HSplit l il l' l'' where
    hSplit l = hSplit' (hMarkAll Ctrl.HLeft l)
class (HListIx il,HList l) => HSplit' l il l' l'' | l il -> l' l'' where
    hSplit' :: l -> il -> (l',l'')
instance HSplit'' l l' l'' => HSplit' l HNil l' l''  where
    hSplit' l _ = hSplit'' l
instance (HUnmark Ctrl.HLeft l i m,HMark Ctrl.HRight m i n,HSplit' n il l' l'')
        => HSplit' l (HCons i il) l' l'' where
    hSplit' l (HCons i il) = hSplit' (hMark Ctrl.HRight (hUnmark Ctrl.hLeft l i) i) il
class HList l => HSplit'' l l' l'' | l -> l' l'' where
    hSplit'' :: l -> (l',l'')
instance HSplit'' HNil HNil HNil where
    hSplit'' _ = (HNil,HNil)
instance HSplit'' l l' l'' => HSplit'' (HCons (Ctrl.HLeft e) l) (HCons e l') l'' where
    hSplit'' (HCons (Ctrl.HLeft e) l) = let (l',l'') = hSplit'' l in (HCons e l',l'')
instance HSplit'' l l' l'' => HSplit'' (HCons (Ctrl.HRight e) l) l' (HCons e l'') where
    hSplit'' (HCons (Ctrl.HRight e) l) = let (l',l'') = hSplit'' l in (l',HCons e l'')

class (HListIx n,HList r) => HDeleteMany r n r' | r n -> r' where
    hDeleteMany :: r -> n -> r'
instance (HMarkAll Ctrl.HLeft l m, HSplit' m il l' l'',HSplit l il l' l'') => HDeleteMany l il l' where
    hDeleteMany l il = fst $ hSplit l il

class (HListIx n,HList r) => HLookupMany r n r' | r n -> r' where
    hLookupMany :: r -> n -> r'
instance HList l => HLookupMany l HNil HNil where
    hLookupMany _ _ = HNil
instance (HLookup l i v,HLookupMany l il l') => HLookupMany l (HCons i il) (HCons v l') where
    hLookupMany l (_ :: HCons i il) = HCons (hLookup l (undefined :: i)) (hLookupMany l (undefined :: il))

class HListIx n => HFindIx n i j | n i -> j where
    hFindIx :: n -> i -> j
instance (HListIx (HCons a n),Peano.Eq a i t,HFindIx' t n i j) => HFindIx (HCons a n) i j where
    hFindIx (HCons a n) i = hFindIx' (a `Peano.eq` i) n i
class HListIx n => HFindIx' t n i j | t n i -> j where
    hFindIx' :: t -> n -> i -> j
instance HListIx n => HFindIx' Logic.AllTrue n i Peano.Zero where
    hFindIx' _ _ _ = Peano.zero
instance HFindIx n i j => HFindIx' Logic.AllFalse n i (Peano.Suc j) where
    hFindIx' _ n i = Peano.Suc $ hFindIx n i

data Label n v = Label n v deriving Show

class HFindLabel ns n i j | ns n i -> j where
    hFindLabel :: (Label ns n) -> (Label ns i) -> j
instance HFindIx n i j => HFindLabel ns n i j where
    hFindLabel (Label _ n) (Label _ i) = hFindIx n i

------------------------------------------------------------------------------
-- Lookups with integral

class HList r => HLookupIntegral r a where
    hLookupIntegral :: Integral i => r -> i -> Maybe a
instance HLookupIntegral HNil a where
    hLookupIntegral _ _ = Nothing
instance HLookupIntegral r a => HLookupIntegral (HCons a r) a where
    hLookupIntegral (HCons a _) 0 = Just a
    hLookupIntegral (HCons _ r) n = hLookupIntegral r (n-1)
instance HLookupIntegral r a => HLookupIntegral (HCons b r) a where
    hLookupIntegral (HCons _ _) 0 = Nothing
    hLookupIntegral (HCons _ r) n = hLookupIntegral r (n-1)

class HList l => HLookupWith l t w where
   hLookupWith :: Integral n => n -> l -> t -> w
instance HLookupWith HNil t w where
   hLookupWith _ _ _ = error "index out of range"
instance (Ctrl.Cont t e w,HLookupWith l t w) => HLookupWith (HCons e l) t w where
   hLookupWith n (HCons _ l) t | n>0 = hLookupWith (n-1) l t
   hLookupWith _ (HCons e _) t = Ctrl.cont t e

class (HList l,Peano.NotNegative i) => HInsert l i e l' | l i e -> l' where
    hInsert :: l -> i -> e -> l'
instance Peano.NotNegative i => HInsert HNil (Peano.Suc i) e (HSingleton e) where
    hInsert _ _ e = hSingleton e
instance HList l => HInsert l Peano.Zero e (HCons e l) where
    hInsert l _ e = HCons e l
instance HInsert l i e l' => HInsert (HCons e' l) (Peano.Suc i) e (HCons e' l') where
    hInsert (HCons e' l) (Peano.Suc i) e = HCons e' (hInsert l i e)

