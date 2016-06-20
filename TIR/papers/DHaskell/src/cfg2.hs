{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

import Prelude hiding (exp,succ)
import CommonMain
import GhcSyntax
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric1
import Label3


-- An illustrative abstract syntax

data Stm   = Assign Ident Exp | Sequ Stm Stm
data Exp   = Var Ident | Zero | Succ Exp
type Ident = String


-- An illustrative concrete syntax

{-

stm ::= ident ":=" exp
stm ::= stm ";" stm
exp ::= ident
exp ::= "Z"
exp ::= "S" exp

-}



-- The reified syntax

data MyNamespace = MyNamespace deriving Show

stm    = firstLabel MyNamespace "stm"
exp    = nextLabel stm "exp"

assign = nextLabel exp "assign"
sequ   = nextLabel assign "sequ"
var    = nextLabel sequ "var"
zero   = nextLabel var "zero"
succ   = nextLabel zero "succ"

syntax = 
     stm .=. (  assign .=. (T ident .*. NT exp .*. HNil)
            .*. sequ   .=. (NT stm .*. NT stm .*. HNil)
            .*. emptyRecord
             )
 .*. exp .=. (  var  .=. (T ident .*. HNil)
            .*. zero .=. HNil
            .*. succ .=. (NT exp .*. HNil)
            .*. emptyRecord
             )
 .*. emptyRecord
 where
  ident   = proxy :: Proxy String

syntax' = grammar syntax


-- The grammar framework

newtype T  x = T x  deriving Show
newtype NT x = NT x

instance ShowLabel x => Show (NT x)
 where
  show (NT x) = showLabel x


-- Minimal grammar check

grammar :: Grammar g => g -> g
grammar =  id

class Grammar g
instance (HZip ls vs r, HLabelSet ls, Nts vs) => Grammar (Record r)
class Nts nts
instance Nts HNil
instance (Nt nt, Nts nts) => Nts (HCons nt nts)
class Nt nt
instance (HZip ls vs r, HLabelSet ls, Prods vs) =>  Nt (Record r)
class Prods r
instance Prods HNil
instance (RhsCompound r, Prods rs) => Prods (HCons r rs)
class HList l => RhsCompound l
instance RhsCompound HNil
instance (RhsBasic e, RhsCompound l) => RhsCompound (HCons e l)
class RhsBasic e
instance RhsBasic (T x)
instance RhsBasic (NT x)


-- Gather defined nonterminals

class DefsGrammar g nts
 where
  defsGrammar :: g -> nts

instance HZip ls vs r => DefsGrammar (Record r) ls
 where
  defsGrammar (Record r) = fst $ hUnzip r
  
------

-- Compute tree type

class AST_Grammar g l t | g l -> t
instance (HZip ls vs r, 
          HLabelSet ls, 
          AST_Nts (Record r) l vs t) 
      => AST_Grammar (Record r) l t

class AST_Nts g l nts t | g l nts -> t
instance (HLookupByHNat n nts nt,  -- lookup the non-terminal by label.
          AST_Nt g nt t)
      => AST_Nts g (Label n ns) nts t

class AST_Nt g nt t | g nt -> t
instance (HZip ls vs r, HLabelSet ls, AST_Prods g vs t)
      => AST_Nt g (Record r) (TIC t)

class AST_Prods g r t | g r -> t
instance AST_Prods g HNil HNil
instance (AST_RhsCompound g r t, AST_Prods g rs ts)
      => AST_Prods g (HCons r rs) (t:+:ts)

class AST_RhsCompound g l t | g l -> t
instance AST_RhsCompound g HNil HNil
instance (AST_RhsBasic g e t, AST_RhsCompound g l ts)
      => AST_RhsCompound g (HCons e l) (t:*:ts)

class AST_RhsBasic g e t | g e -> t
instance AST_RhsBasic g (T x) x
instance AST_Grammar g l t =>
         AST_RhsBasic g (NT l) t


--

ast :: (Grammar g, AST_Grammar g l t) => g -> l -> t
ast g l = undefined

isGrammar :: (Grammar g) => g -> ()
isGrammar g = ()
