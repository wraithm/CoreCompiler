module TI where

import Data.List (mapAccumL)
import Data.Monoid ((<>))

import AST
import Heap
import CorePrelude

-- This is a simple template instantiation graph reducer

data TiState = TiState
    { stack :: TiStack
    , dump  :: TiDump
    , heap  :: TiHeap
    , globals :: TiGlobals
    , stats :: TiStats }
    deriving Show

data TiDump = TiDump deriving Show
type TiStack = [Addr]
type TiHeap = Heap Node
type TiGlobals = [(Name, Addr)]
type TiStats = Int

data Node = NApp Addr Addr -- Application
    | NComb Name [Name] CoreExpr -- Combinator
    | NNum Int -- Numbers
    deriving Show

initialDump = TiDump

incTiStats :: TiState -> TiState
incTiStats state = state { stats = (stats state) + 1 }

buildInitialHeap :: [CoreDefn] -> (TiHeap, [(Name, Addr)])
buildInitialHeap = mapAccumL allocateComb emptyHeap

allocateComb :: TiHeap -> CoreDefn -> (TiHeap, (Name,Addr))
allocateComb heap (name, args, body) = (heap', (name,addr))
  where (heap', addr) = alloc heap (NComb name args body)

compile program = TiState
    { stack = [addrOfMain]
    , dump = initialDump
    , heap = initialHeap
    , globals = initialGlobals
    , stats = 0 } 
  where
    combDefs = program ++ preludeDefs
    (initialHeap, initialGlobals) = buildInitialHeap combDefs
    addrOfMain = findWithDefault 0 "main" initialGlobals

tiFinal :: TiState -> Bool
tiFinal (TiState [a] _ h _ _) = isDataNode $ hLookup h a
tiFinal (TiState [] _ _ _ _) = error "Empty stack!"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _ = False

step :: TiState -> TiState
step state@(TiState (s:_) _ h _ _) = dispatch $ hLookup h s
  where
    dispatch (NNum _) = error "Number applied as function!"
    dispatch (NApp a1 a2) = appStep state a1 a2
    dispatch (NComb comb args body) = cStep state comb args body

appStep state@(TiState s _ _ _ _) a1 _ = state { stack = a1 : s }

cStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
cStep (TiState s d h g st) name args body = TiState s' d h' g st
  where
    s' = resultAddr : drop (length args + 1) s
    (h', resultAddr) = instantiate body h env
    env = argBindings <> g
    argBindings = zip args (getArgs h s)

getArgs h (s:st) = map getArg st
  where getArg addr = let (NApp _ arg) = hLookup h addr in arg

instantiate :: CoreExpr -> TiHeap -> [(Name, Addr)] -> (TiHeap, Addr)
instantiate (Num n) h _ = alloc h (NNum n)
instantiate (App e1 e2) h env = alloc h2 (NApp a1 a2)
  where
    (h1, a1) = instantiate e1 h env
    (h2, a2) = instantiate e2 h1 env
instantiate (Var v) h env = (h, findWithDefault err v env)
  where err = error $ "Undefined name " ++ show v ++ " in " ++ show env
instantiate (Let False defs body) h env = instantiate body h'' env''
  where
    (h'', env'') = foldl (\a d -> addDef a d) (h,env) defs
    addDef (ah, env') (name, e) = (h',(name,addr):env')
      where (h',addr) = instantiate e ah env
-- TODO letrec is not implemented.
instantiate (Let True defs body) h env = instantiate body h'' env''
  where
    (h'', env'') = foldl (\a d -> addDef a d) (h,env) defs
    addDef (ah, env') (name, e) = (h',(name,addr):env')
      where (h',addr) = instantiate e ah env
instantiate (Constr tag arity) h env = error "Can't instantiate constructors... yet..."
instantiate (Case e alts) h env = error "Can't instantiate cases... yet..."
instantiate (Lam _ _) _ _ = error "Must lambda lift program first."

eval state = state : rest
  where
    rest | tiFinal state = []
         | otherwise = eval next
    next = incTiStats (step state)

runProgram prog = hLookup h result
  where 
    final = (last . eval . compile) prog
    result = (head . stack) final
    h = heap final
