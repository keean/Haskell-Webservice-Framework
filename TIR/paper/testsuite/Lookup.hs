Just for the sake of it, here's a little bit simplified version of
your example. Labels are done differently.


>> {-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
>> module Foo where


>> data Z = Z
>> data S a = S a
>>
>> class Nat a where n2n:: a-> Int
>> instance Nat Z where n2n _ = 0
>> instance (Nat a) => Nat (S a) where n2n _ = 1 + n2n (undefined::a)
>>
>> -- keyed access
>> class (Nat n) => MLookup n a r  where
>>    mLookup :: r -> n -> a
>> instance MLookup Z a (a,r) where
>>    mLookup r _ = fst r
>> instance MLookup n a r => MLookup (S n) a (a,r) where
>>    mLookup (_,xs) _ = mLookup xs (undefined::n)
>> instance MLookup n a r => MLookup n a (b,r) where
>>    mLookup (_,xs) n = mLookup xs n
>>
>> -- Positional access
>> class (Nat n) => MIndex n a r | n r -> a where
>>     midx:: r -> n -> a
>> instance MIndex Z a (a,r) where midx (x,_) _ = x
>> instance (MIndex n a r) => MIndex (S n) a (b,r) where
>>     midx (_,xs) _ = midx xs (undefined::n)


The example now reads


>> newtype Name a = Name a deriving Show
>> newtype Size a = Size a deriving Show
>> newtype Weight a = Weight a deriving Show
>>
>> infixr 5 &+
>> (&+) = (,)
>> test = (Name "Box") &+ (Size (3::Int)) &+ (Weight (1.1::Float))
>>        &+ (Name "AnotherBox") &+ (Size (42::Int)) &+ (Weight (24.09::Float))
>>        &+ ()


Note that at run-time, (Name "Box") is the same as "Box". Name is a
compile-time-only label that incurs no run-time overhead. So, the
essence is the same -- a polymorphic associative list where keys are
ephemeral (have no run-time representation). "mLookup r n" finds the
n-th association in this list of a type a. In the present
representation, labels and values are specified together. There is
no need for a type declaration for test. The compiler will figure it
out.

The name and the size of the first box

*Foo> mLookup test Z::(Name String)
Name "Box"
*Foo> mLookup test Z::(Size Int)
Size 3

and of the second one

*Foo> mLookup test (S Z)::(Name String)
Name "AnotherBox"
*Foo> mLookup test (S Z)::(Size Int)
Size 42

We can also access the element of the array by their absolute
position:

*Foo> midx test Z
Name "Box"
*Foo> midx test (S Z)
Size 3
*Foo> midx test (S (S (S Z)))
Name "AnotherBox"

Decimal types for indices (rather than unary, as above)
would make for a nicer interface.

We can easily write a right fold


>> class MFoldr a r  where
>>    mfoldr :: (a -> b -> b) -> b -> r -> b
>> instance (MFoldr a r) => MFoldr a (a,r) where
>>    mfoldr f z r = f (fst r) $ mfoldr f z (snd r)
>> instance MFoldr a () where
>>    mfoldr f z r = z
>> instance MFoldr a r => MFoldr a (b,r) where
>>    mfoldr f z r = mfoldr f z (snd r)


and find out how many names are in our list

*Foo> mfoldr (\ (Name (a::String)) n -> n + 1) 0 test
2

and the total sizes of our boxes

*Foo> mfoldr (\ (Size s) a -> a + s) (0::Int) test
45

Incidentally, this polymorphic array may be a model of an extensible
record with row polymorphism and even polymorphic labels. Yet another
record proposal.

