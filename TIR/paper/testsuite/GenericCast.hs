{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

class  TypeSafeCast a a'
 where typeSafeCast :: a -> Maybe a'

instance TypeSafeCast a a
 where   typeSafeCast = Just
  
instance TypeSafeCast a a'
 where   typeSafeCast = const Nothing
