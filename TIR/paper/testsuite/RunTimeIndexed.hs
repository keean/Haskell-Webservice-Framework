Just running through the implementation of some of the functions I mentioned,
with the newtyped tuple version. Thought the run-time indexed code looked neat
so I thought i'd post it:

First the definition:

newtype RNil = RNil ()
newtype RCons attribute relation = RCons (attribute,relation)

class Relation relation
instance Relation (RNil ())
instance Relation relation => Relation (RCons attribute relation)

emptyRM :: RNil
emptyRM = RNil ()


now the code:

class Relation r => IndexRM' r a where
   indexRM' :: r -> Int -> Maybe a
instance IndexRM' RNil a where
   indexRM' _ _ = Nothing
instance IndexRM' r a => IndexRM' (RCons a r) a where
   indexRM' (RCons (a,_)) 0 = Just a
   indexRM' (RCons (_,r)) n = indexRM' r (n-1)
instance IndexRM' r a => IndexRM' (RCons b r) a where
   indexRM' (RCons (_,_)) 0 = Nothing
   indexRM' (RCons (_,r)) n = indexRM' r (n-1)


This finds the Nth value where 'n' is a normal integer. The last block
repeats twice because one matches the case where the return type is the same, and
the other is for then the return type and indexed value differ. This is why the
return type is Maybe. With the compile time natural number lookup, the program
will fail to compile if the types differ, but with the runtime lookup the type
and the success cannot be determined (hence no functional dependancy) and returning
Maybe makes the most sense.

	Regards,
	Keean.


Havent posted this to the list as it is just a minor change from before,
nothing much to say apart from the use of the Integral class is a bit
neater:


class (Relation r,Integral i) => IndexRM' r i a where
   indexRM' :: r -> i -> Maybe a
instance IndexRM' RNil a where
   indexRM' _ _ = Nothing
instance IndexRM' r a => IndexRM' (RCons a r) i a where
   indexRM' (RCons (a,_)) 0 = Just a
   indexRM' (RCons (_,r)) n = indexRM' r (n-1)
instance IndexRM' r a => IndexRM' (RCons b r) i a where
   indexRM' (RCons (_,_)) 0 = Nothing
   indexRM' (RCons (_,r)) n = indexRM' r (n-1)
