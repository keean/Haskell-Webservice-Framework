{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

import MiniHList
import SingletonHOccurs

instance Cast x x 
 where
  cast = id


{-

*Main> hOccurs (HCons True HNil)
True
*Main> hOccurs (HCons 1 HNil)
1
*Main> :t hOccurs (HCons 1 HNil)
hOccurs (HCons 1 HNil) :: forall e e1.
                          (HOccurs e1 (HCons e HNil), Num e) =>
                          e1
*Main>

-}
