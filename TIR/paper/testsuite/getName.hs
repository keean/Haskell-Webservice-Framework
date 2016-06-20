With reference to my last email, I think I may have jumped the gun...
I was thinking about the code oleg posted... using a type like:

newtype Name = Name {getName :: String}

You could then use:

getName (mLookup test Z)

which uses the type 'deconstructor' to force the return type of the lookup.
I am not sure which I prefer, to utilise the tuple, or to create a new type
to form the list. A new type has added type safety, which after all is why
Im doing this... so this suggests a third intermediate form:

newtype RNil = RNil ()
newtype RCons a r = RCons (a,r)

I wont bother posting any code as it is trivial to modify olegs last
version to use the new types... This version would produce the same
run time code as the last, but differetiates between ordinary tuple
pairs and void value and the relation and empty values.


Currently I think the above form, using deconstructor access provides
the interface I want, but with improved efficiency from using newtype
insead of data. Also it avoids having two complete definitions for the
lists , one with a type index and one without.

One final point ... in relational algebra an attribute implies a domain,
therefore:

newtype Name = Name {getName :: String}

is more appropriate than

newtype Name a = Name {getName :: a}

but it is possible to use both with olegs mLookup implementation, and the
varient above.

Also, naming? After reading Oleg's earlier posts, and the followups, I am
inclined to rename mLookup ... also the way FiniteMaps are handled. What about
the following methods:

assuming the type is calleds a RelationalMap for now,

headRM
tailRM
lastRM
initRM
isEmptyRM
appendRM
prependRM
indexRM
indexRM'  (provides runtime lookup using integers instead of naturals)
mapRM
foldRM
zipRM
unzipRM
projectRM
injectRM

	inject replaces the first type match (if it exists)... 

insertRM

	insert replaces the first type match or appends if no match.

productRM

not sure about tuple conversion... would fromRM/toRM do, should it be more
explicit tupleToRM tupleFromRM ?

liftRM   (lifts a type to a singleton - is this necessary?)
lookupRM (projectRM returns the first match, lookupRM the Nth match_)


So for example:

	r2 `insertRM` (NewName (getName (projectRM r1)))


	Regards,
	Keean.
