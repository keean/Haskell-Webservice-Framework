-- Haskell HTML/SQL module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTML.Colours (ColourModel,RGB(..),HSV(..)) where

import Data.Bits
-- import Lang.Int

class Show a => ColourModel a where
    colour :: (Double,Double,Double) -> a
    toRGB :: a -> RGB

newtype RGB = MkRGB (Double,Double,Double)
newtype HSV = MkHSV (Double,Double,Double)

instance ColourModel RGB where
    colour c = MkRGB c
    toRGB c = c

instance ColourModel HSV where
    colour c = MkHSV c
    toRGB c = hsvToRGB c

instance Show HSV where
    showsPrec _ hsv = shows (toRGB hsv)

instance Show RGB where
    showsPrec _ (MkRGB (r,g,b)) = showChar '#' . toHex (round $ r*255.0) . toHex (round $ g*255.0) . toHex (round $ b*255.0) where

        toHex :: Int -> ShowS
        toHex i 
            | i>255 = showString "FF"
            | i>15 || i<256 = hexChar (i `div` 16) . hexChar (i `mod` 16)
            | i>0 || i<16 = showChar 'F' . hexChar i
            | otherwise = showString "00"

        hexChar :: Int -> ShowS
        hexChar i = showChar $ (['0'..'9']++['A'..'F']) !! i

hsvToRGB :: HSV -> RGB
hsvToRGB (MkHSV (h,s,v)) = (case i `mod` 6 of
    j   | j==0 -> MkRGB (v,n,m)
        | j==1 -> MkRGB (n,v,m)
        | j==2 -> MkRGB (m,v,n)
        | j==3 -> MkRGB (m,n,v)
        | j==4 -> MkRGB (n,m,v)
        | j==5 -> MkRGB (v,m,n)
        | otherwise -> MkRGB (v,v,v)) where

    i :: Int
    i = (floor h)

    f :: Double
    f = h - (fromIntegral i)

    g :: Double
    g = if ((i .&. 1) == 1)
        then f
        else (1.0 - f)

    m :: Double
    m = v * (1.0 - s)

    n :: Double
    n = v * (1.0 - s * g)
