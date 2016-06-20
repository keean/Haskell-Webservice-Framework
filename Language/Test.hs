module Test where

import Numeric

type Answer = (Subst,Integer)
type Subst = [(Vname,Term)]
data Term = Func Fname [Term] | Var Vname deriving Eq
type Fname = String
data Vname = Name String | Auto Integer deriving Eq
type Predicate = Answer -> [Answer]

instance Show Term where
	showsPrec _ t = showTerm t

instance Show Vname where
	showsPrec _ v = showVname v

showAnswer :: Answer -> ShowS
showAnswer (s,i) = showChar '[' . showSubst s . showChar ']'

showSubst :: Subst -> ShowS
showSubst ((x,t):s@(_:_)) = showVname x . showString " -> " . showTerm t . showChar ',' . showSubst s
showSubst ((x,t):_) = showVname x . showString " -> " . showTerm t
showSubst _ = id

showTerm :: Term -> ShowS
showTerm (Func f t) = showString f . if null t then id else (showChar '(' . showTerms t . showChar ')')
showTerm (Var v) = showVname v

showTerms :: [Term] -> ShowS
showTerms (t0:ts@(_:_)) = showTerm t0 . showChar ',' . showTerms ts
showTerms (t0:_) = showTerm t0
showTerms _ = id

showVname :: Vname -> ShowS
showVname (Name n) = showString n
showVname (Auto i) = showInt i

makevar :: Integer -> Term
makevar i = Var (Auto i)

var :: String -> Term
var v = Var (Name v)

nil :: Term
nil = Func "nil" []

atom :: Fname -> Term
atom a = Func a []

func :: Fname -> [Term] -> Term
func f t = Func f t

cons :: Term -> Term -> Term
cons a b = Func "cons" [a,b]

(|?) :: Predicate -> Predicate -> Predicate
(p |? q) x = p x ++ q x

(&?) :: Predicate -> Predicate -> Predicate
p &? q = concat . map q . p

true :: Predicate
true x = [x]

false :: Predicate
false x = [] 

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

isFunc :: Term -> Bool
isFunc (Func _ _) = True
isFunc _ = False

vname :: Term -> Vname
vname (Var v) = v

fname :: Term -> Fname
fname (Func n _) = n

terms :: Term -> [Term]
terms (Func _ ts) = ts

arity :: Term -> Int
arity = length . terms

subst :: Term -> Subst -> Term
subst s ((x,t):sigma)
	| vname s == x = t
	| otherwise = subst s sigma
subst s _ = s

(~>) :: Term -> Term -> (Vname,Term)
(Var s) ~> t = (s,t)

unify :: Subst -> (Term,Term) -> [Subst]
unify sigma (s,t) = let
	s' = if isVar s then subst s sigma else s
	t' = if isVar t then subst t sigma else t
	in if isVar s' && s'==t' then [sigma] else if isFunc s' && isFunc t'
		then if fname s' == fname t' && arity s' == arity t'
			then unify' sigma (terms s') (terms t')
			else []
		else if not (isVar s)
			then unify sigma (t',s')
			else [s' ~> t' : sigma]

unify' :: Subst -> [Term] -> [Term] -> [Subst]
unify' s (t0:ts) (u0:us) = case unify s (t0,u0) of
	s@(_:_) -> unify' (concat s) ts us
	_ -> []
unify' s [] [] = [s]
unify' _ _ _ = []

(=?) :: Term -> Term -> Predicate
(t =? u) (s,n) = [(s',n) | s' <- unify s (t,u)]

exists :: (Term -> Predicate) -> Predicate
exists p (s,n) = p (makevar n) (s,n+1)

solve :: Predicate -> [String]
solve p = map (\a -> showAnswer a "") (p ([],0))

test = solve (var "X" =? func "f" [var "X"])

test2 = solve ((func "f" [atom "a",var "x"] =? func "f" [var "y",atom "b"]) &? (func "f" [atom "a",var "x"] =? func "f" [var "y",atom "b"]))

