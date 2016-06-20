Re: Polymorphic lists...

Hi,

        Just been playing with constraints a bit more... It seems to me
there is a complete Prolog in there (well minus IO and debugging anyway)
An instance with no constraints is equivalent to a Prolog fact, and an
instance with constraints is equivalent to a prolog rule. I have found
references on Haskell mailing lists going back to about 2000, that
state this equivalence, however they either didn't realise what you
could do with this, ot kept it to themselves.

        Here's some fun ones:

class Relation r => Pointless r
instance Pointless Nil
instance (Pointless r,Insert r a r) => Pointless (Cons a r)

Insert updates the type if it exists in the list, or appends if it does not.
So this list will only accept new items that are already items of the list...
therefore it can only ever be (Nil).


Using some new classes for Naturals, and the Logic stuff I posted earlier you
can define:

class (Relation r,Relation r',Nat n) => LessThan r n r' | r n -> r' where
   lessthan :: r -> n -> r'
instance Nat n => LessThan Nil n Nil where
   lessthan _ _ = empty
instance (Relation rr,
      LessThan r n r',
      Lt a n t,
      Prepend r' a (Cons a r'),
      Conditional t r' (Cons a r') rr) => LessThan (Cons a r) n rr where
   lessthan (Cons (a,r)) n = (\r' -> cond (a `lt` n) r' (prepend r' a)) (lessthan r n)


This removes all values less than "n" from the list. Note we are using statically
typed unary numbers here (so this cant be done with a normal list as every value
has a different type). Actually I think GreaterThan would be a better name as it only
returns values Greater(or Equal) to "n" in the output list. Using this we can get:

class Relation r => OrderedList r
instance OrderedList Nil
instance (OrderedList r,LessThan r a r,Nat a) => OrderedList (Cons a r)

Which forces the list to be in ascending order, because if a were greater than
any values in r already, then r /= r in the arguments of the LessThan constraint.


I have also noticed that since defining conditional operators, you don't need
so many instances for classes. Providing the members of the list have an equality
operator (using the multi-typed logic) instances of the pattern:

instance (Cons a r) a (Cons a r')
...
instance (Cons b r) a r'
...

can be replaced with something like:

instance (Eq a b t,Conditional t (Cons a r') r' r'') => (Cons b r) a r''

obviously in the above r' needs to be tied to something, usually the result of a 
recursive call...


	Regards,
	Keean.
