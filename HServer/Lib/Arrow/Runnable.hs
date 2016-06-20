{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- ArrowIO

module Lib.Arrow.Runnable(Runnable(..)) where

--------------------------------------------------------------------
-- classes for generalised reduction.

class Runnable x y where
   run :: x -> y
