module GRMachine where

import Data.List (mapAccumL)

import AST
import Heap
import CorePrelude

data GmState = GmState
    { code  :: GmCode
    , stack :: GmStack
    , heap  :: GmHeap
    , globals :: GmGlobals
    , stats :: GmStats
    } deriving Show

type GmCode = [Instruction]
type GmStack = [Addr]
type GmHeap = Heap Node
type GmGlobals = [(Name, Addr)]
type GmStats = Int

incStats :: GmState -> GmState
incStats s = s { stats = (stats s) + 1 }

data Node = NNum Int
    | NApp Addr Addr
    | NGlobal Int GmCode
    deriving Show

data Instruction = Unwind
    | Mkap
    | PushGlobal Name
    | PushInt Int
    | Push Int
    | Slide Int
    deriving (Show, Eq)

putCode :: GmCode -> GmState -> GmState
putCode i s = s { code = i }

putStack :: GmStack -> GmState -> GmState
putStack st s = s { stack = st }

eval :: GmState -> [GmState]
eval s = s : rest
  where
    rest | gmFinal s = []
         | otherwise = eval next
    next = incStats (step s)

gmFinal :: GmState -> Bool
gmFinal s = code s == []

step :: GmState -> GmState
step s = dispatch i (putCode is s)
  where i:is = code s

dispatch :: Instruction -> GmState -> GmState
dispatch Unwind = unwind
dispatch Mkap = mkap
dispatch (PushGlobal n) = pushGlobal n
dispatch (PushInt n) = pushInt n
dispatch (Push n) = push n
dispatch (Slide n) = slide n

unwind :: GmState -> GmState
unwind s = newState (hLookup h a)
  where
    h = heap s
    (a:as) = stack s
    newState (NNum _) = s
    newState (NApp a1 _) = putCode [Unwind] (putStack (a1:a:as) s)
    newState (NGlobal n c)
        | length as < n = error "Unwinding stack with too few arguments."
        | otherwise = putCode c s

mkap :: GmState -> GmState
mkap s = s { heap = h', stack = a:as' }
  where
    (h', a) = alloc (heap s) (NApp a1 a2)
    (a1:a2:as') = stack s

pushGlobal :: Name -> GmState -> GmState
pushGlobal n s = putStack (a : stack s) s
  where 
    a = findWithDefault err n (globals s)
    err = error $ "Undeclared global " ++ n
    
pushInt :: Int -> GmState -> GmState
pushInt n s = s { heap = h', stack = a : stack s }
  where (h', a) = alloc (heap s) (NNum n)

push :: Int -> GmState -> GmState
push n s = putStack (a:as) s
  where 
    as = stack s
    a = getArg (hLookup (heap s) (as !! (n + 1)))
    getArg (NApp _ a2) = a2

slide :: Int -> GmState -> GmState
slide n s = putStack (a : drop n as) s
  where (a:as) = stack s

compile prog = GmState
    { code = [PushGlobal "main", Unwind]
    , stack = []
    , heap = h
    , globals = g
    , stats = 0 }
  where
    (h, g) = buildInitialHeap prog

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap prog = mapAccumL allocateComb emptyHeap compiled
  where
    compiled = map compileComb (preludeDefs ++ prog) ++ compiledPrimitives

compiledPrimitives = []

type GmCompiledC = (Name, Int, GmCode)

allocateComb :: GmHeap -> GmCompiledC -> (GmHeap, (Name, Addr))
allocateComb h (name, nargs, is) = (h', (name, addr))
  where (h', addr) = alloc h (NGlobal nargs is)

compileComb :: (Name, [Name], CoreExpr) -> GmCompiledC
compileComb (name, env, body) = (name, length env, compileR body (zip env [0..]))

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = [(Name, Int)]

compileR :: GmCompiler
compileR e env = 
    compileC e env ++ 
    [ Slide (length env + 1)
    , Unwind ]

compileC :: GmCompiler
compileC (Var x) env
    | x `elem` (domain env) = [Push n]
    | otherwise = [PushGlobal x]
  where
    domain xs = [ x | (x, _) <- xs ]
    n = findWithDefault err x env
    err = error $ "Could not find " ++ x ++ " in environment " ++ show env
compileC (Num n) env = [PushInt n]
compileC (App e1 e2) env =
    compileC e2 env ++
    compileC e1 (argOffset 1 env) ++
    [ Mkap ]
  where
    argOffset n env = [ (x, n + m) | (x, m) <- env ]

runProgram prog = hLookup h result
  where 
    final = (last . eval . compile) prog
    result = (head . stack) final
    h = heap final
