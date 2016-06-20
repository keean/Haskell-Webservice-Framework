{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

module Rev where

{-
  Non-recursive non-constrained

DHaskell notation:
datatype MayB a = Noth | Some a

We generate the following:

-}

data MayB a = Noth | Some a
-- just collect all the type variables
data CNoth a  = CNoth
data CSome a  = CSome a

-- class C<datatype-name> ( self :: * -> * ) the-rest-of-typevar
class CMayB ( self :: * -> * ) a
instance CMayB CNoth a
instance CMayB CSome a
instance CMayB MayB a

-- Now, we want the following in DHaskell:
-- It is the correct expression in Haskell, too.
mbe_ Noth f g = f
mbe_ (Some a) f g = g a

{-
First, we enter it in Haskell and type it:
*Rev> :t mbe_
mbe_ :: forall t a. MayB a -> t -> (a -> t) -> t

-}

-- We note the type of mbe_ and so we generate
-- we collect all type variables and introduce self for the MayB

class CMayB self a => MBE self a t where
    mbe:: self a -> t -> (a -> t) -> t

instance MBE CNoth a t where
    mbe CNoth f g = f
instance MBE CSome a t where
    mbe (CSome a) f g = g a
instance MBE MayB a t where
    -- that is literally mbe_
    -- So, the value part is identical to the DHaskell expression
    mbe Noth f g = f
    mbe (Some a) f g = g a

test_mbe1 = mbe (CSome 'a') 'x' id
test_mbe2 = mbe CNoth 'x' id
test_mbe3 = mbe (Some 'a') 'x' id

{-
Homogeneous unconstrained recursive
datatype List a = Nil | Cons a (List a)

first we re-write it as

datatype List self a = Nil | Cons a (self a) where self = List self

-}

data List a = Nil | Cons a (List a) deriving Show

-- generate from the re-written part
data CNil self a = CNil deriving Show
data CCons self a = CCons a (self a) deriving Show

class CList ( self :: * -> * ) a
instance CList (CNil self) a
instance CList self a => CList (CCons self) a
instance CList List a


-- Here's waht we want to write in DHaskell
-- It is the correct expression in Haskell, too.

rev_ Nil acc = acc
rev_ (Cons h t) acc = rev_ t (Cons h acc)

{-
*Rev> :t rev_
rev_ :: forall a. List a -> List a -> List a
-}

-- Note: self3 shows up only in the result. So it depends on everything else

class (CList self1 a, CList self2 a, CList self3 a)
    => Rev self1 self2 self3 a | self1 self2 -> self3  where
    rev:: self1 a -> self2 a -> self3 a

instance (CList self2 a) => Rev (CNil self) self2 self2 a where
    rev CNil acc = acc

{-
First we try:
instance Rev (CCons t1) self2 self2 a where
    rev (CCons h t) acc = rev t (CCons h acc)

    Could not deduce (CList self2 a)
	from the context (Rev t1 (CCons self2) self3 a)
      arising from the instance declaration at /tmp/b.hs:98

    Could not deduce (Rev t1 (CCons self2) self2 a)
	from the context (Rev (CCons t1) self2 self2 a)

so we add it. But we had to changed it a bit. GHC didn't generalize well
enough...: cf self2 vs. self3...

So, the idea is to add enough constraints to the intance until GHC shuts up
-}
instance (Rev t1 (CCons self2) self3 a, CList self2 a)
    => Rev (CCons t1) self2 self3 a where
    rev (CCons h t) acc = rev t (CCons h acc)

instance Rev List List List a where
    rev Nil acc = acc
    rev (Cons h t) acc = rev t (Cons h acc)

test_rev1 = rev (Cons 'a' (Cons 'b' (Cons 'c' Nil))) Nil
test_rev2 = rev (CCons 'a' (CCons 'b' (CCons 'c' CNil))) CNil

{-
Homogeneous length-guarded recursive
datatype ListL len a = 
  NilL where len = Zero
| ConsL a (ListL len1 a) where len = Succ len1

first transform into:

datatype ListL self[len1] len a = 
  NilL where len = Zero
| ConsL a (self[len1] a) 
   where self[len1] = ListL self[lenx] len1 and len = Succ len1
-}

data Zero
data Succ a

class Nat a
instance Nat Zero
instance Nat n => Nat (Succ n)
instance Nat Int -- dynamic case


-- Addition of type-level naturals
class (Nat n1, Nat n2, Nat n3) 
    => Add n1 n2 n3 | n1 n2 -> n3 where
    add:: n1 -> n2 -> n3
instance Nat n => Add Zero n n where add = undefined
instance Add n n' n'' => Add (Succ n) n' (Succ n'') where
    add = undefined
instance Add Int Int Int where
    add n1 n2 = n1 + n2

-- Here, the indexing is dynamic
data ListL len a = NilL len | ConsL len a (ListL len a) deriving Show
len_ListL (NilL len) = len
len_ListL (ConsL len _ _) = len

data CNilL self len a = CNilL deriving Show
data CConsL self len a = CConsL a (self a) deriving Show

class CListL ( self :: * -> * ) a

instance CListL (CNilL self Zero) a
instance CListL (self len1) a => CListL (CConsL (self len1) (Succ len1)) a
instance CListL (ListL Int) a

revl_ (NilL len) acc = acc
revl_ (ConsL len h t) acc = revl_ t (ConsL undefined h acc)
{-
*Rev> :t revl_
revl_ :: forall a a1 len. ListL len a -> ListL a1 a -> ListL a1 a
Now, we want that length(ListL a1 a) + length(ListL a2 a) =
  length(ListL a2 a)
-}

class (CListL (self1 len1) a, CListL (self2 len2) a, CListL (self3 len3) a,
       Add len1 len2 len3)
    => RevL self1 len1 self2 len2 self3 len3 a
	| self1 len1 self2 len2 -> self3
	, self1 len1 self2 len2 -> len3 where
    revl:: self1 len1 a -> self2 len2 a -> self3 len3 a

-- we add the constraints until the typechecker stops complaining...
instance (Nat len2, CListL (self2 len2) a)
    => RevL (CNilL self) Zero self2 len2 self2 len2 a where
    revl CNilL acc = acc

{-
 We first start with the following:

instance (CListL self2 len2 a)
    => RevL (CConsL t1) len1 self2 len2 self3 len3 a where
    revl (CConsL h t) acc = revl t (CConsL h acc)

We immediately get the problem: functional dependencies
So, we temporarily disable the dependency and get the problem

    Could not deduce (Add len1 len2 len3,
		      CListL self3 len3 a,
		      CListL (CConsL t1) len1 a)
    Could not deduce (RevL t1 len1 (CConsL self2) len2 self3 len3 a)

And so we add them to make the compiler happy
-}

-- {-
instance (CListL (self2 len2) a,
	  Add len1 len2 len3,
	  RevL t1 len1 (CConsL (self2 len2)) (Succ len2) self3 (Succ len3) a)
    => RevL (CConsL (t1 len1)) (Succ len1) self2 len2 self3 (Succ len3) a where
    revl (CConsL h t) acc = revl t
			    ((CConsL h acc)::CConsL (self2 len2) (Succ len2) a)
-- -}
{-
Bad instance: functional dependencies do not hold
instance (CListL (self2 len2) a,
	  Add len1 len2 len3,
	  RevL t1 len1 (CConsL (self2 len2)) (Succ len2) self3 (Succ len3) a)
    => RevL (CConsL (t1 len1)) (Succ len1) self2 len2 self3 (Succ len3) a where
    revl (CConsL h t) acc = revl t acc
-}

-- Now, generate the dynamic instance
instance RevL ListL Int ListL Int ListL Int a where
    revl l@(NilL len) acc = verify acc
      where verify r | add (len_ListL l) (len_ListL acc) == len_ListL r = r
	    verify r = error "Failed verification"
    revl l@(ConsL len h t) acc = verify $ revl t (ConsL (1+(len_ListL acc)) h acc)
      where verify r | add (len_ListL l) (len_ListL acc) == len_ListL r = r
	    verify r = error "Failed verification"

-- the latter are assured
cnill:: (CListL (CNilL () Zero) a) => CNilL () Zero a
cnill = CNilL

cconsl:: (CListL (t len) a) => a -> t len a -> CConsL (t len) (Succ len) a
cconsl h t = CConsL h t

-- the latter ought to be generated
nill:: ListL Int a
nill = NilL 0
consl h t = ConsL (1 + len_ListL t) h t

test_revl1 = revl (cconsl 'a' (cconsl 'b' (cconsl 'c' cnill))) cnill
test_revl2 = revl (consl 'a' (consl 'b' (consl 'c' nill))) nill


{-
Heterogeneous unconstrained recursive
datatype ListH t a = NilH | ConsH a t where t = (exists a' t'. ListH t' a')

I'm falling asleep... It's 3am!
-}


{-
Some stuff that didn't work out
newtype Nil ls = Nil
data Cons a l = Cons a l

newtype VList a = VList [a]

class List a
instance List Nil
instance List (Vlist a)


class Apply k arg res | k arg -> res where
    apply:: k -> arg -> res

class Cons kl ls a lsp where
    cons :: a -> kl lsp -> (kl ls -> w) -> w
class Decons kl ls a lsp where
    decon_list :: kl ls -> (a -> kl lsp -> w) -> w

class List kl ls where
    nil:: kl ls
    cons:: a -> kl ls -> (forall ls'. List kl ls' => kl ls' -> w) -> w
    decon_list:: kl ls -> w -> (forall ls'. List kl ls' => a -> kl ls' -> w) -> w

class (List kl ls, List kl acc, List kl r, Apply k (kl r) w) 
    => Reverse kl ls acc r k w where
    revers :: kl ls -> kl acc -> k -> w

instance (List kl ls, List kl acc, List kl r,
	  Apply k (kl acc) w, Apply k (kl r) w)
    => Reverse kl ls acc r k w where
    revers ls acc k = decon_list ls
			   ((apply k acc)::w)
			   (\h t -> cons h acc (\acc' -> revers t acc' k))
-}
