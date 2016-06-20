{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-} 

newtype W a = W a
 
--
-- This class constraint is occassionally useful.
-- class Cast' (W x) (W (Maybe y)) => Cast x y where
--
class Cast x y where
  cast :: x -> Maybe y

instance Cast x x
 where
  cast = Just

instance Cast x y
 where
  cast _ = Nothing

{-
instance Cast' (W x) (W (Maybe y)) => Cast x y where
  cast x = case cast' (W x) of (W y) -> y

class Cast' x y where
  cast' :: x -> y

instance Cast' (W x) (W (Maybe x)) where
  cast' (W x) = (W (Just x))

instance Cast'' x y => Cast' x y where
  cast' x = cast'' x

class Cast'' x y where
  cast'' :: x -> y

instance Cast'' (W x) (W (Maybe y)) where
  cast'' _ = W Nothing
-}

data ExistType = forall t . 
        ( Cast t String,
          Cast t Int,
          Cast t Float
        ) => ExistType t


elist :: [ExistType]
elist = [ ExistType "Hello"
        , ExistType (1::Int)
        , ExistType "World"
        , ExistType (1.2::Float)
        ]


showExist :: [ExistType] -> IO ()
showExist [] = return ()
showExist (ExistType t0:ts) = do
   case cast t0 of
      Just (t :: String) -> putStrLn $ showString "STRING: " . show $ t
      Nothing -> case cast t0 of
         Just (t :: Int) -> putStrLn $ showString "INT: " . show $ t
         Nothing -> case cast t0 of
            Just (t :: Float) -> putStrLn $ showString "FLOAT: " . show $ t
            Nothing -> putStrLn "UNKNOWN"
   showExist ts

main = do _ <- print $ ( cast True :: Maybe Bool 
                       , cast True :: Maybe Int
                       )
          showExist elist
