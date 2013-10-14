module GraphReduction.Compiler where

import Data.List (mapAccumL)

import Core.AST
import Core.Prelude
import Util.Heap

import GraphReduction.Machine

-- Compile for the GR Machine

runProgram :: CoreProgram -> Node
runProgram prog = hLookup h result
  where 
    final = (last . eval . compile) prog
    result = (head . stack) final
    h = heap final

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


-- The compiler

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
    [ Update d
    , Pop d
    , Unwind ]
  where
    d = length env

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [ (x, n + m) | (x, m) <- env ]

compileC :: GmCompiler
compileC (Var x) env
    | x `elem` domain env = [Push n]
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
compileC (Let isRec defs e) args
    | isRec = compileLetrec compileC defs e args
    | otherwise = compileLet compileC defs e args
compileC x env = error $ "Can't compile: " ++ show x ++ " with " ++ show env

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env = zip (map fst defs) [n-1, n-2 .. 0] ++ argOffset n env
  where n = length defs

compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs e env
  = compileLet' defs env ++ comp e env' ++ [Slide (length defs)]
  where
    env' = compileArgs defs env
    compileLet' [] _ = []
    compileLet' ((_, expr):defs) env
      = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileLetrec comp defs e env 
  = [Alloc n] ++ compileLetrec' defs env 1 ++ comp e env' ++ [Slide n]
  where
    n = length defs
    env' = compileArgs defs env
    compileLetrec' [] _ _ = []
    compileLetrec' ((_, expr):defs) env m
      = compileC expr env ++ [Update (n - m)] ++ compileLetrec' defs (argOffset 1 env) (m + 1)
