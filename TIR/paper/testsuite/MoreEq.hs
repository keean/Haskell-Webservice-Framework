{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}


class Force   a b | a -> b where force:: a->b
class Force'  t a b | t a -> b where force':: t->a->b
class Force'' t a b | t a -> b where force'':: t->a->b
instance Force'  () a b => Force a b where force x = force' () x
instance Force'' t a b => Force' t a b where force' = force''
instance Force'' () a a where force'' _ x  = x

class TypeEq x y
 where
  typeEq :: x -> y -> ()

instance TypeEq x x
 where
  typeEq _ _ = ()

data Ex = forall x. Ex x
data Un = Un (forall x. x)

class PTypeEq x y | x -> y
 where
  pTypeEq :: x -> y -> ()

instance PTypeEq x x
 where
  pTypeEq _ _ = ()

class TypeSafeCast a a'
 where
  typeSafeCast :: a -> Maybe a'
 
instance TypeSafeCast a a
 where
  typeSafeCast = Just
 
instance TypeSafeCast a a'
 where
  typeSafeCast = const Nothing

class Cast x y | x -> y
 where
  cast :: x -> y

instance Cast x x 
 where
  cast = id
