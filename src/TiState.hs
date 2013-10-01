module TiState where

import qualified Data.Map.Strict as M
import Data.List (mapAccumL)

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

type TiStack = [Addr]
data TiDump = TiDump
type TiHeap = Heap Node
type TiGlobals = M.Map Name Addr
type TiStats = Int

data Node = NApp Addr Addr -- Application
    | NComb Name [Name] CoreExpr -- Combinator
    | NNum Int -- Numbers

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
    (initialHeap, gs) = buildInitialHeap combDefs
    initialGlobals = M.fromList gs
    addrOfMain = M.findWithDefault 0 "main" initialGlobals

-- TODO Implement object lookup from the heap
tiFinal :: TiState -> Bool
tiFinal (TiState [a] _ h _ _) = case M.lookup a (objects h) of
    Just x -> isDataNode x
    Nothing -> error "Could not find last address!"
tiFinal (TiState [] _ _ _ _) = error "Empty stack!"
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _ = False

step :: TiState -> TiState
step state@(TiState (s:_) _ h _ _) = case M.lookup s (objects h) of
    Just x -> dispatch x
    Nothing -> error "Could not find value on the stack!"
  where
    dispatch (NNum _) = error "Number applied as function!"
    dispatch (NApp a1 a2) = appStep state a1 a2
    dispatch (NComb comb args body) = cStep state comb args body

    appStep state@(TiState s _ _ _ _) a1 a2 = state { stack = a1 : s }

    cStep _ _ _ _ = undefined

eval state = 
    state : rest
  where
    rest | tiFinal state = []
         | otherwise = eval next
    next = incTiStats (step state)
