{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module TTypeLift where

import TTypeable

import Char 
import Numeric
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import GHC.IOBase

------------------------------------------------------------------------------

class TApply z a b | z a -> b where
	tapply :: z -> a -> b

class TTuple x y

------------------------------------------------------------------------------
-- types above have no values and represent true type level
-- constructs. Types below have values...

class Constraint c v w | c v -> w where
	constraint :: c -> v -> w
	constraint _ _ = undefined

class Constrain c v w z | c v w -> z where
	constrain :: c -> v -> w -> z
instance (Constraint c v t,Constrain' t v w z) => Constrain c v w z where
	constrain c v w = constrain' (constraint c v) v w

class Constrain' c t f z | c t f -> z where
	constrain' :: c -> t -> f -> z
instance Constrain' TTrue t f t where
	constrain' _ t _ = t
instance Constrain' TFalse t f f where
	constrain' _ _ f = f

data ANY = ANY
instance Constraint ANY v TTrue where
	constraint _ _ = ttrue

data x:/\:y = x:/\:y
instance (Constraint x a u,Constraint y a v,TAnd u v w) => Constraint (x:/\:y) a w where
	constraint (x:/\:y) a = constraint x a `tAnd` constraint y a

data x:\/:y = x:\/:y
instance (Constraint x a u,Constraint y a v,TOr u v w) => Constraint (x:\/:y) a w where
	constraint (x:\/:y) a = constraint x a `tOr` constraint y a

data Cons v k = Cons v k deriving Show
data Nil = Nil deriving Show

class List c k where
{-
	list :: c -> k -> k
	list _ k = k
-}
instance List c Nil
instance (Constraint c v TTrue,List c k) => List c (Cons v k)

cons :: v -> k -> Cons v k
cons v k = Cons v k

nil :: Nil
nil = Nil

data TJust x = TJust x deriving Show
data TNothing x = TNothing deriving Show

class TMaybe c v where
instance Constraint c v TTrue => TMaybe c (TNothing v) where
instance Constraint c v TTrue => TMaybe c (TJust v) where

class TCast x m y | x y -> m	where
	tCast :: x -> m y
instance TCast' x m y => TCast x m y where
	tCast x = tCast' x undefined
class TCast' x m y | x y -> m where
	tCast' :: x -> y -> m y
instance (TypeEq x y t,TCast'' t x m y) => TCast' x m y where
	tCast' x y = tCast'' (typeEq x y :: t) x
class TCast'' t x m y | t -> m where
	tCast'' :: t -> x -> m y
instance TCast'' TTrue x TJust x where
	tCast'' _ x = TJust x
instance TCast'' TFalse x TNothing y where
	tCast'' _ _ = TNothing

------------------------------------------------------------------------------
-- lifting types

ttypegen :: String -> Int -> Q [Name]
ttypegen s i
	| i>0 = do
		m <- newName s
		l <- ttypegen s (i-1)
		return (m:l)
	| otherwise = return []

ttyperhs :: [Strict] -> [Name] -> [StrictTypeQ]
ttyperhs (s0:ss) (n:ns) = strictType (return s0) (varT n) : ttyperhs ss ns
ttyperhs _ _ = []

ttypecon :: [Name] -> Con -> Q [Dec]
ttypecon ks (ForallC vs _ t) = ttypecon' ks vs t
ttypecon ks t@(NormalC _ _) = ttypecon' ks [] t

ttypecon' :: [Name] -> [Name] -> Con -> Q [Dec]
ttypecon' ks vs (NormalC n t) = do
	u <- sequence $ map (ttypevar (ks++vs) . snd) t
	let v = concat u in do
		d <- dataD (cxt []) n v [normalC n $ map (\x -> return (NotStrict,VarT x)) v] [''Show]
		return [d]

ttypevar :: [Name] -> Type -> Q [Name]
ttypevar vs (VarT u)
	| u `elem` vs = return [u]
	| otherwise = do
		n <- (newName . nameBase) u
		return [n]
ttypevar vs (ForallT ws _ t) = do
	m <- ttypevar (vs++ws) t
	return m
ttypevar _ _ = do
	n <- newName "z"
	return [n]

ttypeapp :: [StrictType] -> TypeQ
ttypeapp ((_,t0):ts@(_:_)) = appT (return t0) (ttypeapp ts)
ttypeapp ((_,t0):_) = return t0
ttypeapp _ = error "ttypeapp: empty type"

 
ttypecxt :: ([Name],Type) -> Q ([Dec],[Type])
ttypecxt (_,VarT _) = return ([],[])
ttypecxt (v,ForallT n c t) = do
	(y,l) <- ttypecxt (v++n,t)
	return (y,c++l)
ttypecxt (v,t) = do
	a <- ttypeexp (tail v) (head v) t
	return a

conname :: Type -> Name
conname (ConT c) = c
conname (AppT a _) = conname a

-- look for "Constr a ... z" if any args are Constrs,
-- then replace with new tyvar and separate.
ttypeexp :: [Name] -> Name -> Type -> Q ([Dec],[Type])
ttypeexp _ v (AppT ListT x)
	| isCon x = let z = conname x in do
			l <- return $ mkName $ "List_" ++ nameBase z
			w <- appT (conT l) (varT v)
			y <- liftListType z l
			return (y,[w])
ttypeexp m v (AppT a b@(ConT _)) = do
	n <- newName "z"
	(x,t) <- ttypeexp m v a
	(y,u) <- ttypecxt (n:m,b)
	w <- appT (return $ head t) (varT n)
	return (x++y,w:tail t++u)
ttypeexp m v (AppT a b@(AppT _ _))
	| isTuple b = do
		(x,t) <- ttypeexp m v a
		(y,u) <- ttypecxt (m,b)
		w <- appT (return $ head t) (return $ head u)
		return (x++y,w:tail t ++ tail u)
	| otherwise = do
		n <- newName "z"
		(x,t) <- ttypeexp m v a
		(y,u) <- ttypecxt (n:m,b)
		w <- appT (return $ head t) (varT n) 
		return (x++y,w:tail t++u)
ttypeexp m v (AppT a (VarT u)) = do
	(y,t) <- ttypeexp m v a
	w <- appT (return $ head t) (varT u)
	return (y,w:tail t)
ttypeexp _ v (ConT n) = do
	w <- appT (conT n) (varT v)
	return ([],[w])
ttypeexp _ v ArrowT = do
	w <- appT (conT ''TApply) (varT v)
	return ([],[w])
ttypeexp _ _ t = return ([],[t])

isTuple :: Type -> Bool
isTuple (AppT a _) = isTuple a
isTuple (TupleT _) = True
isTuple _ = False

isList :: Type -> Bool
isList (AppT a _) = isList a
isList ListT = True
isList _ = False

isArrow :: Type -> Bool
isArrow (AppT a _) = isArrow a
isArrow ArrowT = True
isArrow _ = False

ttypeins :: Name -> [Name] -> Con -> Q [Dec]
ttypeins c ks (ForallC vs cx t) = ttypeins' c ks vs cx t
ttypeins c ks t@(NormalC _ _) = ttypeins' c ks [] [] t

ttypeins' :: Name -> [Name] -> [Name] -> Cxt ->  Con -> Q [Dec]
ttypeins' c ks vs cx (NormalC n t) = do
	u <- sequence $ map (ttypevar (ks++vs) . snd) t
	let v = concat u in do
		b <- sequence $ map ttypecxt $ zip u (map snd t)
		let
			(y,w) = unzip b
			(p,r) = separate w
			in	do
				i <- instanceD
					(return $ r ++ cx)
					(applyTyVars (appT (conT c) (ttypeinst (conT n) (reverse v) p)) $ reverse ks)
					[]
				return (i:concat y)

ttypeinst :: TypeQ -> [Name] -> [Maybe Type] -> TypeQ
ttypeinst q (_:ns) (Just v0:vs) = appT (ttypeinst q ns vs) (return v0)
ttypeinst q (n:ns) (Nothing:vs) = appT (ttypeinst q ns vs) (varT n)
ttypeinst q [] [] = q

separate :: [[Type]] -> ([Maybe Type],[Type])
separate (t@(t0:ts):tts)
	| isTuple t0 = (Just t0:as,ts++bs)
	| otherwise = (Nothing:as,t++bs)
	where (as,bs) = separate tts
separate (_:tts) = (Nothing:as,bs)
	where (as,bs) = separate tts
separate _ = ([],[])

ttypedrv :: Con -> Q [Dec]
ttypedrv (NormalC n _) = ttypeable n

liftListType :: Name -> Name -> Q [Dec]
liftListType constraintName listName = do
	k <- newName "k"
	v <- newName "v"
	c <- classD (cxt []) listName [k] [] []
	iNil <- instanceD (cxt []) (appT (conT listName) (conT ''Nil)) []
	iCons <- instanceD
		(cxt [appT (conT listName) (varT k),appT (conT constraintName) (varT v)])
		(appT (conT listName) (appT (appT (conT ''Cons) (varT v)) (varT k)))
		[]
	return [c,iNil,iCons]

ttypebld :: [Name] -> [Con] -> Name -> Q [Dec]
ttypebld v t n = do
	u <- sequence $ map (ttypecon v) t
	k <- newName "k"
	c <- classD (cxt []) n (k:v) [] []
	i <- sequence $ map (ttypeins n v) t
	d <- sequence $ map (ttypeableMapName qualifyName) (concat u)
	return $ c : concat u ++ concat i ++ concat d

ttypesyn :: [Name] -> Type -> Name -> Q [Dec]
ttypesyn v t n = do
	k <- newName "k"
	(y,w) <- ttypecxt (k:v,t)
	let (p,r) = separate [w] in do
		c <- classD (cxt []) n (k:v) [] []
		i <- instanceD (return r)
			(applyTyVars (conT n) $ reverse (k:v))
			[]
		return ([c,i] ++ y)

ttypedec :: Dec -> Q [Dec]
ttypedec (DataD _ n v t _) = ttypebld v t n
ttypedec (NewtypeD _ n v t _) = ttypebld v [t] n
ttypedec (TySynD n v t) = ttypesyn v t n
ttypedec (FunD n c) = liftFunction n c
ttypedec (SigD _ _) = return []
ttypedec (ClassD _ _ _ _ _) = return []

ttypelift :: Q [Dec] -> Q [Dec]
ttypelift q = do
	tDecShow q
	d <- q
	c <- sequence $ map ttypedec d
	runIO . putStrLn . pprint $ (concat c)
	return $ concat c

------------------------------------------------------------------------------
-- lifting functions

getUniqueTyVars :: [Pat] -> [Name] -> Q [Name]
getUniqueTyVars (VarP tv:ps) ns = do
	if not (tv `elem` ns)
		then getUniqueTyVars ps (tv:ns)
		else getUniqueTyVars ps ns
getUniqueTyVars (TupP ts:ps) ns = getUniqueTyVars (ts++ps) ns
getUniqueTyVars (ConP _ ts:ps) ns = getUniqueTyVars (ts++ps) ns
getUniqueTyVars (InfixP p0 _ p1:ps) ns = getUniqueTyVars (p0:p1:ps) ns
getUniqueTyVars (TildeP p:ps) ns = getUniqueTyVars (p:ps) ns
getUniqueTyVars (AsP _ p:ps) ns = getUniqueTyVars (p:ps) ns
getUniqueTyVars (WildP:ps) ns = do
	w <- newName "w"
	getUniqueTyVars ps (w:ns)
getUniqueTyVars (RecP _ fs:ps) ns = getUniqueTyVars (getRecTyVars fs ++ ps) ns
getUniqueTyVars (ListP ts:ps) ns = getUniqueTyVars (ts++ps) ns
getUniqueTyVars (SigP p _:ps) ns = getUniqueTyVars (p:ps) ns
getUniqueTyVars _ ns = return $ reverse ns

getRecTyVars :: [FieldPat] -> [Pat]
getRecTyVars ((_,p):ps) = p:getRecTyVars ps
getRecTyVars _ = []

mkClassArgs :: [Pat] -> Q [Name]
mkClassArgs (_:ps) = do
	v0 <- newName "t"
	vs <- mkClassArgs ps
	return (v0:vs)
mkClassArgs _ = return []

defineClass :: Name -> [Name] -> DecQ
defineClass cn ns = do
	z <- newName "z"
	classD (cxt []) cn (ns++[z]) [FunDep ns [z]] []

toClassName :: Name -> Q Name
toClassName n = return $ mkName ((toUpper . head) b : tail b)
	where b = nameBase n

defineInstance :: Name -> [Pat] -> Body -> DecQ
defineInstance name pats (NormalB body) = do
	ct <- expToType body
	vr <- getUniqueTyVars pats []
	runIO . putStrLn . show $ ct
	(rt,cv) <- ttypeToTyp vr ct
	instanceD (cxt $ map return cv) (appT (patsToType (reverse pats) (ConT name)) (return rt)) []

patsToType :: [Pat] -> Type -> TypeQ
patsToType (ListP [p]:ps) t = appT (patsToType ps t) (appT (patsToType [p] (ConT ''Cons)) (conT ''Nil))
patsToType (VarP p0:ps) t = appT (patsToType ps t) (varT p0)
patsToType (ConP tc tvs:ps) t 
	| tc == '[] = appT (patsToType ps t) (patsToType (reverse tvs) (ConT ''Nil))
	| otherwise = appT (patsToType ps t) (patsToType (reverse tvs) (ConT tc))
patsToType (WildP:ps) t = do
	w <- newName "w"
	appT (patsToType ps t) (varT w)
patsToType (InfixP p0 tc p1:ps) t 
	| tc == '(:) = appT (patsToType ps t) (patsToType [p1,p0] (ConT ''Cons))
	| otherwise = appT (patsToType ps t) (patsToType [p1,p0] (ConT tc))
patsToType [] t = return t

expToType :: Exp -> TypeQ
expToType (ListE l) = foldl (\a b -> appT a (expToType b)) listT l
expToType (VarE x) = varT x
expToType (ConE x)
	| x == '[] = conT ''Nil
	| otherwise = conT x
expToType (AppE x y) = appT (expToType x) (expToType y)
expToType (InfixE (Just x) (ConE f) (Just y))
	| f == '(:) = appT (appT (conT ''Cons) (expToType x)) (expToType y)
	| otherwise = appT (appT (conT f) (expToType x)) (expToType y)

-- get outermost type constructor or variable
-- if a function move to cxt and add return type
ttypeToTyp :: [Name] -> Type -> Q (Type,[Type])
ttypeToTyp _ ListT = return (ConT ''Nil,[])
ttypeToTyp _ (ConT v)
	| v == '[] = return (ConT ''Nil,[])
	| v == '(:) = return (ConT ''Cons,[])
	| otherwise = return (ConT v,[])
ttypeToTyp _ (VarT v) = return (VarT v,[])
ttypeToTyp n (AppT x y) 
	| isCon x = do
		(a,b) <- ttypeToTyp n x 
		(c,d) <- ttypeToTyp n y
		return (AppT a c,b++d)
	| isVar x = do
		z <- newName "z"
		return (VarT z,[AppT (AppT (upperT n x) y) (VarT z)])
	| isList x = do
		(a,b) <- ttypeToTyp n x
		(c,d) <- ttypeToTyp n y
		return (AppT (AppT (ConT ''Cons) a) c,b++d)

upperT :: [Name] -> Type -> Type
upperT n (VarT v)
	| v `elem` n = AppT (ConT ''TApply) (VarT v)
	| otherwise = let w = nameBase v
		in ConT (mkName (toUpper (head w):tail w))
upperT n (AppT x y) = AppT (upperT n x) y
upperT _ t = t

isVar :: Type -> Bool
isVar (AppT x _) = isVar x
isVar (VarT _) = True
isVar _ = False

isCon :: Type -> Bool
isCon (AppT x _) = isCon x
isCon (ConT _) = True
isCon _ = False

liftFunction :: Name -> [Clause] -> Q [Dec]
liftFunction n c@(Clause pats _ _:_) = do
	runIO . putStrLn . show $ n
	-- tv <- getUniqueTyVars pats []
	cn <- toClassName n
	tv <- mkClassArgs pats
	runIO . putStrLn . show $ tv
	cd <- defineClass cn tv
	runIO . putStrLn . pprint $ cd
	is <- liftClauses cn c
	return (cd:is)
liftFunction _ _ = return []

liftClauses :: Name -> [Clause] -> Q [Dec]
liftClauses cn (Clause pats body decs:cs) = do
	i0 <- defineInstance cn pats body 
	runIO . putStrLn . pprint $ i0
	is <- liftClauses cn cs
	return (i0:is)
liftClauses _ _ = return []

------------------------------------------------------------------------------
-- utility functions

tDecShow :: Q [Dec] -> Q [Dec]
tDecShow q = do
	d <- q
	runIO . putStrLn . show $ d
	return []

tTypeShow :: Q Type -> Q [Dec]
tTypeShow q = do
	t <- q
	runIO . putStrLn . show $ t
	return []

ttypeshow :: Name -> Q [Dec]
ttypeshow q = do
	r <- reify q
	case r of
		(TyConI t) -> runIO . putStrLn . show $ t
		_ -> return ()
	return []
