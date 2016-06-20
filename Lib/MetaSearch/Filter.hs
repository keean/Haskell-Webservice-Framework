-- filter.hs (C)2001 Keean Schupke
--
--      Polymorphic monadic filter

module Lib.MetaSearch.Filter(MonadFilter(..),FilterDOM(..),filterDOM,FilterElem(..),ElemResult(..),DOMResult(..),
    return,(>>=),mzero,mplus,fmap,graft,prune,remove,from,to,alter,fork,addTag,null,select,moveExc,moveInc,cut,
    trimElem,moveAll,mkFilterDOM,dropClose,moveBranch,down,tidyDOM,moveThisBranch,selectMod,discardBranches) where

import Prelude hiding (null)
--import Data.Char
import Control.Monad
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
import GHC.Base

-- filters differ from parsers in that a parser consumes the string, returning
-- successful parse results, whereas a filter modifies the state (string) and
-- normaly returns a Bool - indicating if the state was changed.

-- element filters ----------------------------------------

newtype FilterElem a = FilterElem (Elem -> ElemResult a)
data ElemResult a = ElemOut a Elem | ElemVoid

{-# INLINE filterElem #-}
filterElem :: FilterElem a -> (Elem -> ElemResult a)
filterElem (FilterElem e) = e

instance Functor FilterElem where
    {-# INLINE fmap #-}
    fmap g (FilterElem f) = FilterElem (\e -> case f e of
        ElemOut a e' -> ElemOut (g a) e'
        _ -> ElemVoid)

instance Applicative FilterElem where
    pure x = FilterElem (\s -> ElemOut x s)
    (FilterElem mf) <*> (FilterElem mx) = FilterElem $ \e -> case mf e of
        ElemOut f e' -> case mx e' of
            ElemOut x e'' -> ElemOut (f x) e''
            _ -> ElemVoid
        _ -> ElemVoid

instance Monad FilterElem where
    {-# INLINE return #-}
    return a    = FilterElem (\s -> ElemOut a s)
    {-# INLINE (>>=) #-}
    p >>= f = FilterElem (\s -> case filterElem p s of
        ElemOut a s' -> filterElem (f a) s'
        _ -> ElemVoid)

instance Alternative FilterElem where
    empty = FilterElem $ \_ -> ElemVoid
    (FilterElem f) <|> (FilterElem g) = FilterElem $ \s -> case f s of
        out@(ElemOut _ _) -> out
        _ -> g s

instance MonadPlus FilterElem where
    {-# INLINE mzero #-}
    mzero = FilterElem (\_ -> ElemVoid)
    {-# INLINE mplus #-}
    p `mplus` q = FilterElem (\s -> case filterElem p s of
        out@(ElemOut _ _) -> out
        _ -> filterElem q s)

{-# INLINE trimElem #-}
trimElem :: FilterElem ()
trimElem = FilterElem (\e -> case e of
    (MkElem (Text,t,a)) -> case trimText t of
        t' | isBlankText t' -> ElemVoid
           | otherwise -> ElemOut () (MkElem (Text,t',a))
    _ -> ElemOut () e)

{-# INLINE dropClose #-}
dropClose :: FilterElem ()
dropClose = FilterElem (\e ->case e of
    (MkElem (Close,_,_)) -> ElemVoid
    _ -> ElemOut () e)

{-# INLINE tidyDOM #-}
tidyDOM :: FilterElem ()
tidyDOM = do
    trimElem
    dropClose
    return ()

-- DOM filters --------------------------------------------

newtype FilterDOM a = FilterDOM (DOM -> DOMResult a)
data DOMResult a = DOMOut a DOM | DOMVoid

{-# INLINE filterDOM #-}
filterDOM :: FilterDOM a -> (DOM -> DOMResult a)
filterDOM (FilterDOM f) = f

instance Functor FilterDOM where
    {-# INLINE fmap #-}
    fmap g (FilterDOM f) = FilterDOM (\s -> case f s of
        DOMOut a s' -> DOMOut (g a) s'
        _ -> DOMVoid)


instance Applicative FilterDOM where
    pure x = FilterDOM $ \s -> DOMOut x s
    FilterDOM mf <*> FilterDOM mx = FilterDOM $ \s -> case mf s of
        DOMOut f s' -> case mx s' of
            DOMOut x s'' -> DOMOut (f x) s''
            _ -> DOMVoid
        _ -> DOMVoid

instance Monad FilterDOM where
    {-# INLINE return #-}
    return a    = FilterDOM (\s -> DOMOut a s)
    {-# INLINE (>>=) #-}
    p >>= f = FilterDOM (\s -> case filterDOM p s of
        DOMOut a s' -> filterDOM (f a) s'
        _ -> DOMVoid)


instance Alternative FilterDOM where
    empty = FilterDOM $ \_ -> DOMVoid
    FilterDOM f <|> FilterDOM g = FilterDOM $ \s -> case f s of
        out@(DOMOut _ _) -> out
        _ -> g s

instance MonadPlus FilterDOM where
    {-# INLINE mzero #-}
    mzero = FilterDOM (\_ -> DOMVoid)
    {-# INLINE mplus #-}
    p `mplus` q = FilterDOM (\s -> case filterDOM p s of
        out@(DOMOut _ _) -> out
        _ -> filterDOM q s)

class (MonadPlus m) => MonadFilter m where
    failIfEmpty :: m ()
    tryFilter :: m [a] -> m [a]
    maybeFilter :: m a -> m (Maybe a)
    with :: DOM -> m a -> m a
    move :: m a -> m DOM
    just :: m a -> m DOM

instance MonadFilter FilterDOM where
    -- returns void if DOM empty
    {-# INLINE failIfEmpty #-}
    failIfEmpty = FilterDOM (\u -> case u of
        (_:_) -> DOMOut () u
        _ -> DOMVoid)

    -- if filter fails return empty DOM
    {-# INLINE tryFilter #-}
    tryFilter (FilterDOM f) = FilterDOM (\u -> case f u of
        DOMVoid -> DOMOut [] u
        other -> other)

    {-# INLINE maybeFilter #-}
    maybeFilter (FilterDOM f) = FilterDOM (\u -> case f u of
        DOMVoid -> DOMOut Nothing u
        DOMOut a r -> DOMOut (Just a) r)

    -- use d as filter input
    {-# INLINE with #-}
    with d (FilterDOM f) = FilterDOM (\u -> case f d of
        DOMOut r _ -> DOMOut r u
        _ -> DOMVoid)

    -- move the result of the filter to lhs. rhs=input
    {-# INLINE move #-}
    move (FilterDOM f) = FilterDOM (\u -> case f u of
        DOMOut _ u' -> case u' of
            (_:_) -> DOMOut u' u
            _ -> DOMVoid
        _ -> DOMVoid)

    -- move the result of the filter to lhs. rhs=[]
    {-# INLINE just #-}
    just (FilterDOM f) = FilterDOM (\u -> case f u of
        DOMOut _ u' -> case u' of
            (_:_) -> DOMOut u' []
            _ -> DOMVoid
        _ -> DOMVoid)

-- Filter definitions -------------------------------------

{-# INLINE mkFilterDOM #-}
mkFilterDOM :: (DOM -> DOM) -> FilterDOM ()
mkFilterDOM f = FilterDOM (\z -> DOMOut () (f z))

_cut :: Int -> DOM -> DOM
_cut i d = case d of
    ((j,e):d')
        | j >= i -> (j-i,e):_cut i d'
        | otherwise -> _cut i d'
    _ -> []

cut :: Int -> FilterDOM ()
cut i = mkFilterDOM (_cut i)

down :: [Int] -> FilterDOM ()
down ns = do
    select ns
    cut 1
    return ()

-- creates new root with first tags that satisfy f on
-- each branch
_graft' :: (Elem -> Bool) -> Int -> DOM -> DOM
_graft' b i d = case d of
    ((j,e):d')
        | j > i -> (j-i,e):_graft' b i d'
        | otherwise -> _graft b d
    _ -> []

_graft :: (Elem -> Bool) -> DOM -> DOM
_graft b d = case d of
    ((i,e):d')
        | b e -> (0,e):_graft' b i d'
        | otherwise -> _graft b d'
    _ -> []

{-# INLINE graft #-}
graft :: (Elem -> Bool) -> FilterDOM ()
graft b = mkFilterDOM (_graft b)

-- chops tree off just before first tag that satisfies f on each branch
_prune' :: (Elem -> Bool) -> Int -> DOM -> DOM
_prune' b i d = case d of
    ((j, _):d')
        | j > i -> _prune' b i d'
        | otherwise -> _prune b d
    _ -> []

_prune :: (Elem -> Bool) -> DOM -> DOM
_prune b d = case d of
    (ie@(i,e):d')
        | b e -> _prune' b i d'
        | otherwise -> ie:_prune b d'
    _ -> []

{-# INLINE prune #-}
prune :: (Elem -> Bool) -> FilterDOM ()
prune b = mkFilterDOM (_prune b)

-- removes nodes which satisfy f
_remove :: (Elem -> Bool) -> Int -> [Int] -> DOM -> DOM
_remove b j k@(k0:k') d = case d of
    ((i,e):d')
        | b e -> if (i-j) < k0
            then _remove b (j-1) k' d
            else _remove b (j+1) (i:k) d'
        | otherwise -> if (i-j) < k0
            then _remove b (j-1) k' d
            else (i-j,e):_remove b j k d'
    _ -> []
_remove b _ [] d = case d of
    ((i,e):d')
        | b e -> _remove b 1 [i] d'
        | otherwise -> _remove b 0 [] d'
    _ -> []

{-# INLINE remove #-}
remove :: (Elem -> Bool) -> FilterDOM ()
remove b = mkFilterDOM (_remove b 0 [])

-- keeps nodes after 'b' 
_from :: (Elem -> Bool) -> DOM -> DOM
_from b d = case d of
    ((_, e):d')
        | b e -> d'
        | otherwise -> _from b d'
    _ -> []

{-# INLINE from #-}
from :: (Elem -> Bool) -> FilterDOM ()
from b = mkFilterDOM (_from b)

-- keeps nodes before 'b' 
_to :: (Elem -> Bool) -> DOM -> DOM
_to b d = case d of
    (ie@(_, e):d')
        | b e -> []
        | otherwise -> ie:_to b d'
    _ -> []

{-# INLINE to #-}
to :: (Elem -> Bool) -> FilterDOM ()
to b = mkFilterDOM (_to b)

-- applies element filter to each element in a DOM
_alter :: FilterElem () -> Int -> [Int] -> DOM -> DOM
_alter b j k@(k0:k') d = case d of
    ((i,e):d') -> case filterElem b e of
        ElemOut () e' -> if (i-j) < k0
            then _alter b (j-1) k' d
            else (i-j,e'):_alter b j k d'
        _ -> if (i-j) < k0
            then _alter b (j-1) k' d
            else _alter b (j+1) (i:k) d'
    _ -> []
_alter b _ [] d = case d of
    ((i,e):d') -> case filterElem b e of
        ElemOut () e' -> (i,e'):_alter b 0 [] d'
        _ -> _alter b 1 [i] d'
    _ -> []

{-# INLINE alter #-}
alter :: FilterElem () -> FilterDOM ()
alter f = mkFilterDOM (_alter f 0 [])

-- use multiple filters on same datastream

_fork :: [FilterDOM ()] -> DOM -> DOM
_fork [] _ = []
_fork (f0:f') d = case filterDOM f0 d of
    DOMOut _ d' -> d'++_fork f' d
    _ -> _fork f' d

{-# INLINE fork #-}
fork :: [FilterDOM ()] -> FilterDOM ()
fork f = mkFilterDOM (_fork f)

_addTag :: Elem -> DOM -> DOM
_addTag _ [] = []
_addTag e d@(_:_) = (0,e):moveRoot 1 d

{-# INLINE addTag #-}
addTag :: Elem -> FilterDOM ()
addTag e = mkFilterDOM (_addTag e)

_null :: DOM -> DOM
_null _ = []

{-# INLINE null #-}
null :: FilterDOM ()
null = mkFilterDOM _null

{-# INLINE discardBranches #-}
discardBranches :: Int -> FilterDOM ()
discardBranches i = mkFilterDOM (tbr i)

tbr :: Int -> DOM -> DOM
tbr _ [] = []
tbr i ((k,_):ds) = tbr' (i-1) k ds

tbr' :: Int -> Int -> DOM -> DOM
tbr' _ _ [] = []
tbr' x i d@((k,_):ds)
    | k > i = tbr' x i ds
    | x > 0 = tbr' (x-1) k ds
    | otherwise = d

{-# INLINE selectMod #-}
selectMod :: Int -> [Int] -> FilterDOM ()
selectMod base sList = mkFilterDOM (sm 0 sList) where

    sm :: Int -> [Int] -> DOM -> DOM
    sm _ _ [] = []
    sm i [] ((k, _):ds) 
        | k==0 = if (i+1)==base
            then sm 0 sList ds
            else sm (i+1) [] ds
        | otherwise = sm i [] ds
    sm i j@(j0:js) (d0@(k, _):ds)
        | k==0 && i==j0 = if (i+1)==base
            then d0:getBranch 0 sList ds
            else d0:getBranch (i+1) js ds
        | k==0 = if (i+1)==base 
            then sm 0 sList ds
            else sm (i+1) j ds
        | otherwise = sm i j ds

    getBranch :: Int -> [Int] -> DOM -> DOM
    getBranch _ _ [] = []
    getBranch i [] (d0@(k, _):ds)
        | k==0 = if (i+1)==base
            then sm 0 sList ds
            else sm (i+1) [] ds
        | otherwise = d0:getBranch i [] ds
    getBranch i j@(j0:js) (d0@(k, _):ds)
        | k==0 && i==j0 = if (i+1)==base
            then d0:getBranch 0 sList ds
            else d0:getBranch (i+1) js ds
        | k==0 = if (i+1)==base
            then sm 0 sList ds
            else sm (i+1) js ds
        | otherwise = d0:getBranch i j ds
        
_select_mod' :: Int -> Int -> [Int] -> [Int] -> DOM -> DOM
_select_mod' _ _ _ _ [] = []
_select_mod' m i n j (ke@(k, _):d')
    | k>0 = ke:_select_mod' m i n j d'
    | otherwise = _select_mod m i n j d'

_select_mod :: Int -> Int -> [Int] -> [Int] -> DOM -> DOM
_select_mod _ _ _ _ [] = []
_select_mod m i n [] d@((k, _):d')
    | k==0 && i==m = _select_mod m 0 n n d
    | k==0 = _select_mod m (i+1) n [] d'
    | otherwise = _select_mod m i n [] d'
_select_mod m i n j@(j0:j') d@(ke@(k, _):d')
    | k==0 && i==m = _select_mod m 0 n n d
    | k==0 && i==j0 = ke:_select_mod' m (i+1) n j' d'
    | k==0 = _select_mod m (i+1) n j d'
    | otherwise = _select_mod m i n j d'

-- keep only elements listed (numbered from i)
_select' :: Int -> [Int] -> DOM -> DOM
_select' _ _ [] = []
_select' i [] (d0@(k, _):d')
    | k > 0 = d0:_select' i [] d'
    | otherwise = []
_select' i j@(j0:js) (d0@(k, _):d')
    | k > 0 = d0:_select' i j d'
    | i==j0 = d0:_select' i js d'
    | otherwise =_select i j d'

_select :: Int -> [Int] -> DOM -> DOM
_select _ [] _ = []
_select _ _ [] = []
_select i j@(j0:j') (ke@(k ,_):d')
    | k==0 = if i==j0 
        then ke:_select' (i+1) j' d'
        else _select (i+1) j d'
    | otherwise = _select i j d'

{-# INLINE select #-}
select :: [Int] -> FilterDOM ()
select i = mkFilterDOM (_select 0 i)

moveExc :: (Elem -> Bool) -> FilterDOM DOM
moveExc b = FilterDOM (\d -> case d of
    (ie@(_, e):d')
        | b e -> DOMOut [] d
        | otherwise -> case filterDOM (moveExc b) d' of
            DOMOut x d'' -> DOMOut (ie:x) d''
            _ -> DOMOut [ie] d'
    _ -> DOMVoid)

moveInc :: (Elem -> Bool) -> FilterDOM DOM
moveInc b = FilterDOM (\d -> case d of
    (ie@(_, e):d')
        | b e -> DOMOut [ie] d'
        | otherwise -> case filterDOM (moveInc b) d' of
            DOMOut x d'' -> DOMOut (ie:x) d''
            _ -> DOMOut [ie] d'
    _ -> DOMVoid)

{-# INLINE moveAll #-}
moveAll :: FilterDOM DOM
moveAll = FilterDOM (\d -> DOMOut d [])

moveThisBranch :: FilterDOM DOM
moveThisBranch = do
    i <- FilterDOM (\d -> case d of
        ((i,_):d') -> DOMOut i d'
        _ -> DOMVoid)
    moveBranch i
    
moveBranch :: Int -> FilterDOM DOM
moveBranch j = FilterDOM (\d -> case d of
    (ie@(i, _):d')
        | i<=j -> DOMOut [] d
        | otherwise -> case filterDOM (moveBranch j) d' of
            DOMOut x d'' -> DOMOut (ie:x) d''
            _ -> DOMOut [ie] d'
    _ -> DOMVoid)

