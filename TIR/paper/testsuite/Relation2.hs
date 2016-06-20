infixr 1 `MCons`
data MNil = MNil deriving Show
data MCons l a r = a `MCons` r deriving Show

class MRelation r
instance MRelation MNil
instance MRelation r => MRelation (MCons l a r)


This is a labelled list. It uses a phantom type to label
each node, but it theoretically needs no storage for the label...

Standard list functions operate as before, and ignore the phantom
type.


class MRelation r => MList r where
   mHead :: MCons l a r -> a
   mTail :: MCons l a r -> r
   mIsEmpty :: r -> Bool
instance MList MNil where
   mHead (x `MCons` _) = x
   mTail (_ `MCons` _) = MNil
   mIsEmpty MNil = True
instance MList r => MList (MCons l a r) where
   mHead (x `MCons` _) = x
   mTail (_ `MCons` xs) = xs
   mIsEmpty (_ `MCons` _) = False


I have the usual suspects fold/map/zip/unzip, and natural
number lookup coded. This is the nice touch though, an indexed
lookup on the phatom type, which finds the Nth occurance of a
given type in the list:


class (MRelation r,Nat n) => MLookup l n r a | l n r -> a where
   mLookup :: r -> l -> n -> a
instance MRelation r => MLookup l Zero (MCons l a r) a where
   mLookup (x `MCons` _) _ Zero = x
instance MLookup l n r b => MLookup l (Suc n) (MCons l a r) b where
   mLookup (_ `MCons` xs) l (Suc n) = mLookup xs l n
instance MLookup l n r b => MLookup l n (MCons m a r) b where
   mLookup (_ `MCons` xs) l n = mLookup xs l n



heres an example:



data Name = Name
data Size = Size
data Weight = Weight

test = MCons Name String
	(MCons Size Int
	(MCons Weight Float
	MNil))
test = "Box" `MCons` 3 `MCons` 1.1

putStrLn $ show $ mLookup test Name zero
putStrLn $ show $ mLookup test Size zero
putStrLn $ show $ mLookup test Weight zero

putStrLn $ show $ mLookup (test `mProduct` test) Name one 
putStrLn $ show $ mLookup (test `mProduct` test) Weight zero



These last two show how to access the labels from the right
and left sides of the product when some/all domains have the
same name.
