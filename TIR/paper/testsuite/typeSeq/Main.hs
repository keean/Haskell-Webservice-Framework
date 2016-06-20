{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}

import HType
import HBool
import HNat
import HList
import HArray
import HTypeDriven
import HTypeIndexed
import HZip
import HSet
import TIP
import TIR
import TIC
import TIM
import GHC.IOBase

myHList = (HCons (42::Int)
          (HCons "s1"
          (HCons (3.14::Float)
          (HCons "s2"
          (HCons True
          HNil)))))

myHCrush = unsafePerformIO
        $ hFoldr (HSeq HShow) 
                 (return () :: IO ()) 
                 myHList

main = print $ ( hOccursStar myHList
                 :: [String]
               )
