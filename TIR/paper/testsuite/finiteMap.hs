data MNil = MNil deriving (Show,Data,Typeable)
data MCons l a r = MCons l a r deriving (Show,Data,Typeable)

class MLookup l r a | l r -> a where
   mLookup :: r -> l -> a
instance MLookup l (MCons l a r) a where
   mLookup (MCons _ x _) _ = x
instance MLookup l r b => MLookup l (MCons m a r) b where
   mLookup (MCons _ _ xs) l = mLookup xs l


This is indexed by a unique type, but stores a second independant
type. The allows a kind of static finite map, which is pretty cool!
Here's an example:

data TmId = TmId
data TmVal = TmVal
data TmFloat = TmFloat
data TmName = TmName

testMap :: MCons TmId Int
	(MCons TmVal String
	(MCons TmFloat Float
	(MCons TmName String
	MNil)))

testMap = MCons TmId 1
	$ MCons TmVal "Hello"
	$ MCons TmFloat 1.2
	$ MCons TmName "World"
	MNil

main :: IO ()
main = do
	putStrLn $ show $ testMap `mLookup` TmId
	putStrLn $ show $ testMap `mLookup` TmVal
	putStrLn $ show $ testMap `mLookup` TmFloat
	putStrLn $ show $ testMap `mLookup` TmName
