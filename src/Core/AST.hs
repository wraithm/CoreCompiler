module Core.AST where

import Data.Maybe (fromMaybe)

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
    deriving Show

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

-- Simple helper tool for list dictionaries
findWithDefault x k = fromMaybe x . lookup k
