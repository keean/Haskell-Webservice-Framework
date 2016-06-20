{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

module EEQ where


import CommonMain hiding (HDeleteMany, hDeleteMany, TypeCast,typeCast)
import GhcSyntax
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric1
import Label4

data W = forall s. Show s => W s

test1 = typeEq (W True) (W True)
-- HTrue

test2 = typeEq (W True) (W ())
-- HTrue

test3 = case (W ()) of W s -> typeEq s s
-- HTrue

test3' = case (W ()) of W s -> typeEq [s] [s]
-- HTrue

test3'' = case (W ()) of W s -> typeEq [s] (Just s)
-- HFalse

{-
test4 = case (W ()) of W s -> case (W ()) of W s1 -> typeEq s s1

    Could not unambiguously deduce (TypeEq s1 s b)
	from the context (Show s)
      arising from use of `typeEq' at /tmp/e.hs:33
    The choice of (overlapping) instance declaration
	depends on the instantiation of `s1, s, b'
-}

--test5 = case (W ()) of W s -> case (W ()) of W s1 -> typeEq s [s1]
-- Ditto


data WE a b = forall s. TypeEq s a b => WE s

teste1 = case ((WE ())::WE () HTrue) of WE s -> typeEq s s

teste1' = case (WE ()) of ((WE s)::WE () HTrue) -> typeEq s ()

-- Error! Good
-- teste1'' = case (WE ()) of ((WE s)::WE () HFalse) -> typeEq s ()

teste2 = case (WE ()) of 
	   ((WE (s::s))::WE () HTrue) -> 
               case (WE 'a') of ((WE s1)::WE s HFalse) -> 
                                  (typeEq s1 s, typeEq s s, typeEq s1 s1)
--(HFalse,HTrue,HTrue)

