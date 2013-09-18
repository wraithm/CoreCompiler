module AST where

type Program a = [Defn a]
type CoreProgram = Program Name

type Defn a = (Name, [a], Expr a)
type CoreDefn = Defn Name

type CoreExpr = Expr Name

data Expr a = Var Name
    | Num Int
    | Constr Int Int -- Constructor tag arity
    | App (Expr a) (Expr a)
    | Let IsRec [(a, Expr a)] (Expr a)
    | Case (Expr a) [Alter a]
    | Lam [a] (Expr a)
    deriving (Show)

type Name = String
type IsRec = Bool

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- Get variables and definitions out of a let expr
bindersOf :: [(a,b)] -> [a]
bindersOf defs = [ name | (name, _) <- defs ]

rhssOf :: [(a,b)] -> [b]
rhssOf defs = [ rhs | (_, rhs) <- defs ]

isAtomic :: Expr a -> Bool
isAtomic (Var _) = True
isAtomic (Num _) = True
isAtomic _ = False

{-
instance Show a => Show (Expr a) where
    show (Num n) = show n
    show (Var x) = x
    show (App e1 e2) = show e1 ++ " " ++ show e2
    show (Let True defs inexp) = "let rec " ++ concatMap (\(x,e) -> show x ++ " = " ++ show e ++ "\n") defs ++ "\nin " ++ show inexp
    show (Let False defs inexp) = "let " ++ concatMap (\(x,e) -> show x ++ " = " ++ show e ++ "\n") defs ++ "\nin " ++ show inexp
    show (Constr i j) = "Pack " ++ show i ++ " " ++ show j
    show e
        | isAtomic e = show e
        | otherwise = "(" ++ show e ++ ")"
-}
