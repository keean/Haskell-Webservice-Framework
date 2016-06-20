{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- A weird type of Castables

data Castable    = forall x. Castable
                 ( forall y. HTypeSafeCast x y => (x, Maybe y)
                 )


-- Feels too polymorphic
mkCast :: forall x. x -> Castable
mkCast x = Castable (x,hTypeSafeCast x)


-- Does not type check
unCast :: Castable -> Maybe String
unCast (Castable (_, y)) = y


-- Type-safe cast
class HTypeSafeCast a a'
 where
  hTypeSafeCast :: a -> Maybe a'
 
instance HTypeSafeCast a a
 where
  hTypeSafeCast = Just
 
instance HTypeSafeCast a a'
 where
  hTypeSafeCast = const Nothing
