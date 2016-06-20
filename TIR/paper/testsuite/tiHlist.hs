However, the following is a more succinct realization of a polymorphic
list. Retrieval is done by ordinary _integers_. The list behaves
roughly as a regular list. Although we use the type of a value to
obtain the integral index, and we use the integral index to fetch the
value.


>>{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
>>
>> module Foo where
>>
>> class (Show a) => TH a b where
>>     idx:: a -> b -> Int
>>     alter:: a -> b -> b
>>     tke:: a -> b -> (forall u v. TH u v => u -> v -> w) -> w
>>
>> instance (Show a) => TH a (a,x) where
>>    idx x y = 0
>>    alter x (_,y) = (x,y)
>>
>> instance (TH a c) => TH a (b,c) where
>>    idx x y = 1 + idx x (undefined::c)
>>    alter x (h,t) = (h,alter x t)
>>
>> data W = W Int deriving Show
>>
>> instance (TH a (a,b), TH W b) => TH W (a,b) where
>>     tke (W 0) th@(h,t) f = f h th
>>     tke (W n) (h,t) f = tke (W$ n-1) t f
>>
>> instance TH W () where
>>     tke _ _ _ = error "Not found"


That's it. Class Show is for expository purposes, so we can print what
we've got.

The following is to enable us to store functional values


>> instance Show (a->b) where show _ = "<fn>"


Here's the initial heap


>> infixr 5 &+
>>
>> a &+ b = (a,b)
>>
>> th1 = (1::Int) &+ 'x' &+ (Just True) &+ () &+ [1.0::Float]
>>       &+ (\(c::Char) -> True)
>>       &+ () -- just to mark the end of it


Now the fun begins:

*Foo> idx 'a' th1
1
*Foo> idx [2.0::Float] th1
4
*Foo> idx (=='c') th1
5

We can use a functional type as an index. We can fetch things too:

*Foo> tke (W 0) th1 (const.show)
"1"
*Foo> tke (W 1) th1 (const.show)
"'x'"
*Foo> tke (W 4) th1 (const.show)
"[1.0]"
*Foo> tke (W 5) th1 (const.show)
"<fn>"

We can store and retrieve things

*Foo> tke (W 4) (alter [1.0::Float, 2.0::Float] th1) (const.show)
"[1.0,2.0]"

Right fold can be done along the lines of tke. Only a type like W
will hold our accumulator.

