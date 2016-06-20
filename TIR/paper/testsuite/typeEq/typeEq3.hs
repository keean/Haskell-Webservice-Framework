{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

{-

This encoding tries to take advantage of the fact that instance
selection is done syntactically via looking at instance heads, and
constraint checking comes afterwards. So to avoid that ...

TypeNotEq Int Int 

... holds we catch "TypeNotEq x x" via an extra instance that is made
to fail with a "Fail x" constraint. This should really work and it
does with ghc and hugs!

The only bad taste about this solution is that we get a strange error
message ... something referring to "Fail ()". We could agree on this to
mean that someone has done "!,fail" in Prolog.

-}

class Fail a
class TypeNotEq x y
 -- the method is for testing only
 where
  typeNotEq :: x -> y -> Bool
  typeNotEq _ _ = True

instance Fail () => TypeNotEq x x
instance TypeNotEq x y
