module Core.LambdaLift where

import Data.Set (Set,(\\))
import qualified Data.Set as S
import Data.List (mapAccumL)

import Core.AST

-- Annotated Expr

type AnnExpr a b = (b, AnnExpr' a b)
freeVarsOf = fst

data AnnExpr' a b = AVar Name
    | ANum Int
    | AConstr Int Int
    | AApp (AnnExpr a b) (AnnExpr a b)
    | ALet Bool [AnnDefn a b] (AnnExpr a b)
    | ACase (AnnExpr a b) [AnnAlt a b]
    | ALam [a] (AnnExpr a b)
    deriving Show

type AnnDefn a b = (a, AnnExpr a b)
type AnnAlt a b = (Int, [a], AnnExpr a b)
type AnnProgram a b = [(Name, [a], AnnExpr a b)]

{-
instance Show a => Show (AnnExpr a b) where
    show (ANum n) = show n
    show (AVar x) = x
    show (AApp e1 e2) = show e1 ++ " " ++ show e2
    show (Let True defs inexp) = "let rec " ++ concapMap (\(x,e) -> show x ++ " = " ++ show e ++ "\n") defs ++ "\nin " ++ show inexp
    show (Let False defs inexp) = "let " ++ concapMap (\(x,e) -> show x ++ " = " ++ show e ++ "\n") defs ++ "\nin " ++ show inexp
    show (Constr i j) = "Pack " ++ show i ++ " " ++ show j
    show e
        | isAtomic = show e
        | otherwise = "(" ++ show e ++ ")"
-}

type NameSupply = Int

initialNameSupply :: NameSupply
initialNameSupply = 0

getName :: NameSupply -> String -> (NameSupply, String)
getName ns prefix = (ns + 1, makeName prefix ns)

getNames :: NameSupply -> [String] -> (NameSupply, [String])
getNames ns prefixes = (ns + 1 + length prefixes, zipWith makeName prefixes [ns..])

makeName :: String -> NameSupply -> String
makeName prefix ns = prefix ++ "_" ++ show ns

lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectCs . rename . abstract . freeVars

freeVars :: CoreProgram -> AnnProgram Name (Set Name)
freeVars prog = [ (name, args, freeVarsE (S.fromList args) body) | (name, args, body) <- prog ]

freeVarsE :: Set Name -> CoreExpr -> AnnExpr Name (Set Name)
freeVarsE _ (Num n) = (S.empty, ANum n)
freeVarsE lv (Var x)
    | x `S.member` lv = (S.singleton x, AVar x)
    | otherwise       = (S.empty, AVar x)
freeVarsE lv (App e1 e2) = (freeVarsOf e1' `S.union` freeVarsOf e2', AApp e1' e2')
  where
    e1' = freeVarsE lv e1
    e2' = freeVarsE lv e2
freeVarsE lv (Lam args body) = (freeVarsOf body' \\ S.fromList args, ALam args body')
  where
    body' = freeVarsE lv' body
    lv' = lv `S.union` S.fromList args
freeVarsE lv (Let isRec defs body) = (defsFree `S.union` bodyFree, ALet isRec defs' body')
  where
    binders = S.fromList $ bindersOf defs
    body_lv = lv `S.union` binders
    rhs_lv = if isRec then body_lv else lv
    rhss' = map (freeVarsE rhs_lv) (rhssOf defs)
    defs' = zip (bindersOf defs) rhss'
    freeInVals = S.unions (map freeVarsOf rhss')
    defsFree
        | isRec = freeInVals \\ binders
        | otherwise = freeInVals
    body' = freeVarsE body_lv body
    bodyFree = freeVarsOf body' \\ binders
freeVarsE lv (Case e alts) = error "Deal with cases later!"
freeVarsE lv (Constr _ _) = error "Deal with constructors later!"

freeVarsOfAlt :: AnnAlt Name (Set Name) -> Set Name
freeVarsOfAlt (_, args, rhs) = freeVarsOf rhs \\ S.fromList args

abstract :: AnnProgram Name (Set Name) -> CoreProgram
abstract prog = [ (name, args, abstractE rhs) | (name, args, rhs) <- prog ]

abstractE :: AnnExpr Name (Set Name) -> CoreExpr
abstractE (_, AVar x) = Var x
abstractE (_, ANum n) = Num n
abstractE (_, AApp e1 e2) = App (abstractE e1) (abstractE e2)
abstractE (_, ALet isRec defs body) 
  = Let isRec [ (name, abstractE body) | (name, body) <- defs ] (abstractE body)
abstractE (free, ALam args body) = foldl App c (map Var fvs)
  where
    fvs = S.toList free
    c = Let False [("c",cRhs)] (Var "c")
    cRhs = Lam (fvs ++ args) (abstractE body)
abstractE (_, ACase _ _) = error "Deal with cases later!"
abstractE (_, AConstr _ _) = error "Deal with constructors later!"

rename :: CoreProgram -> CoreProgram
rename prog = snd (mapAccumL renameC initialNameSupply prog)
  where
    renameC ns (name, args, rhs) = (ns'', (name, args', rhs'))
      where
        (ns', args', env) = newNames ns args
        (ns'', rhs') = renameE env ns' rhs

newNames ns oldNames = (ns', newNames, env)
  where
    (ns', newNames) = getNames ns oldNames
    env = zip oldNames newNames

renameE :: [(Name, Name)] -> NameSupply -> CoreExpr -> (NameSupply, CoreExpr)
renameE env ns (Var x) = (ns, Var (findWithDefault x x env))
renameE env ns (Num n) = (ns, Num n)
renameE env ns (App e1 e2) = (ns'', App e1' e2')
  where
    (ns', e1') = renameE env ns e1
    (ns'', e2') = renameE env ns' e2
renameE env ns (Lam args body) = (ns'', Lam args' body')
  where
    (ns', args', env') = newNames ns args
    (ns'', body') = renameE (env' ++ env) ns' body
renameE env ns (Let isRec defs body) = (ns''', Let isRec (zip binders' rhss') body')
  where
    (ns', body') = renameE bodyenv ns body
    (ns'', binders', env') = newNames ns' (bindersOf defs)
    bodyenv = env' ++ env
    (ns''', rhss') = mapAccumL (renameE rhsEnv) ns'' (rhssOf defs)
    rhsEnv 
        | isRec = bodyenv
        | otherwise = env
renameE _ _ (Case _ _) = error "Deal with cases later!"
renameE _ _ (Constr _ _) = error "Deal with constructors later!"

collectCs :: CoreProgram -> CoreProgram
collectCs = concatMap collectOne
  where collectOne (name, args, rhs) = (name, args, rhs') : cs
          where (cs, rhs') = collectCsE rhs 

collectCsE :: CoreExpr -> ([CoreDefn], CoreExpr)
collectCsE (Num n) = ([], Num n)
collectCsE (Var x) = ([], Var x)
collectCsE (App e1 e2) = (cs' ++ cs'', App e1' e2')
  where
    (cs', e1') = collectCsE e1
    (cs'', e2') = collectCsE e2
collectCsE (Lam args body) = (cs, Lam args body')
  where (cs, body') = collectCsE body
collectCsE (Constr i j) = ([], Constr i j)
collectCsE (Case e alts) = (cse ++ csalts, Case e' alts')
  where
    (cse, e') = collectCsE e
    (csalts, alts') = mapAccumL collectCsAlt [] alts
    collectCsAlt cs (t, args, rhs) = (cs ++ csRhs, (t, args, rhs'))
      where (csRhs, rhs') = collectCsE rhs
collectCsE (Let isRec defs body) =
    (rhssCs ++ bodyCs ++ localCs, mkLet isRec nonCs' body')
  where
    isLam (Lam _ _) = True
    isLam _ = False
    mkLet r d b = if null d then b else Let r d b

    (rhssCs, defs') = mapAccumL collectCsDef [] defs
    cs' = [ (name, rhs) | (name, rhs) <- defs', isLam rhs ]
    nonCs' = [ (name, rhs) | (name, rhs) <- defs', not (isLam rhs) ]
    localCs = [ (name, args, body) | (name, Lam args body) <- cs' ]
    (bodyCs, body') = collectCsE body

collectCsDef cs (name, rhs) = (cs ++ rhsCs, (name, rhs'))
  where (rhsCs, rhs') = collectCsE rhs
