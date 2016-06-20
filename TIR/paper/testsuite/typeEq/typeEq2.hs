{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

Both ghc and hugs think that functional dependencies are not met.

One could wonder if this should not better work.  All what it needs is
a simple overlapping instance rule affecting functional dependency
checking.

See Keean's story below.

-}

data TrueValue  = TrueValue  deriving Show
data FalseValue = FalseValue deriving Show

class TVL x
instance TVL TrueValue
instance TVL FalseValue

class TVL b => TypeEq x y b | x y -> b
 where
  typeEq :: x -> y -> b

instance TypeEq x x TrueValue
 where
  typeEq _ _ = TrueValue

instance TypeEq x y FalseValue
 where
  typeEq _ _ = FalseValue

{-

Keean wrote:

One way out of this mess seems to be to use the most specific instance
in the case of overlapping instances, This would allow the naieve type
equality and delete functions to work, without using the two class
trick. With this in place you could then afford to combine functional
dependancies statically checking for multiple matches ... Of course
this will have to honour the most specific matching rule too soL

        class Equal a b t | a b -> t where
           equal :: a -> b -> t
        instance Equal a a True where
           equal _ _ = true
        instamce Equal a b False where
           equal _ _ = false

Would pass functional dependancy testing on the basis that if a==b
then only instance one is used (although two match, the second is
never used), and is a/=b then only the second rule will be used. In
other words the only way to fail the functional dependancy test is to
have two conflicting rules that are equally specific.

Infact we already have a similar situation, in that only one instance
will bo chosen from the overlapping instances (even though the above
example does not work), so I think the same argument applies. If only
one instances matches based purely on the RHS of the '=>', then if two
constaints on different instances overlap, they don't at the top level
because for all uses we find only one instance to use.  for example:

class TypeEq' (W x) y t => TypeEq x y t | x y -> t where
   typeEq :: x -> y -> t
instance TypeEq' (W x) y t => TypeEq x y t where
   typeEq x y = typeEq' (W x) y
class Tristate t => TypeEq' x y t | x y -> t where
   typeEq' :: x -> y -> t
instance TypeEq' (W x) x True where
   typeEq' _ _ = true
instance TypeEq'' x y t => TypeEq' x y t where
   typeEq' x y = typeEq'' x y
class Tristate t => TypeEq'' x y t | x y -> t where
   typeEq'' :: x -> y -> t
instance TypeEq'' (W x) y False where
   typeEq'' _ _ = false

Even though typeEq'' would match (Bool Bool -> False) and (Bool Int ->
False) in typeEq' we choose the most specific match (Bool Bool ->
True) and never use typeEq''. To me this seems similar to the code:

       if typeX == typeY
           then True
           else if (typeX == typeY)
	      then False
	      else False

And this simplifies to:

	if typeX == typeY then True else False

Likewise with the fundeps, we really want the fundep to apply to the
simplified form of the class. This suggests to me that if we can
select one and only one instance at the top level, and likewise for
the constaint classes, this is sufficient to satisfy the fundep,
providing each constraint class also satisfies its own fundeps, and
the fundeps do not conflict.

	Regards,
	Keean.

-}
