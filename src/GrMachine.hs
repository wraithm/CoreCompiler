module GrMachine where

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
    | NInd Addr
    deriving (Show, Eq)

data Instruction = Unwind
    | Mkap
    | PushGlobal Name
    | PushInt Int
    | Push Int
    | Pop Int
    | Slide Int
    | Update Int
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
dispatch (Pop n) = pop n
dispatch (Slide n) = slide n
dispatch (Update n) = update n

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
    newState (NInd a1) = putCode [Unwind] (putStack (a1:as) s)

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
pushInt n s = case lookup sn (globals s) of
    Nothing -> s { globals = (sn, a) : globals s, heap = h, stack = a : stack s }
      where (h, a) = alloc (heap s) (NNum n)
    Just a -> putStack (a : stack s) s
  where
    sn = show n

push :: Int -> GmState -> GmState
push n s = putStack (a:as) s
  where 
    as = stack s
    a = getArg (hLookup (heap s) (as !! (n + 1)))
    getArg (NApp _ a2) = a2

pop :: Int -> GmState -> GmState
pop n s = putStack (drop n (stack s)) s

slide :: Int -> GmState -> GmState
slide n s = putStack (a : drop n as) s
  where (a:as) = stack s

update :: Int -> GmState -> GmState
update n s = s { stack = newStack , heap = h }
  where
    (a:as) = stack s
    (h, an') = alloc (heap s) (NInd a)
    newStack = case splitAt n as of
        (xs,_:ys) -> xs ++ an' : ys
        (xs,[]) -> xs ++ [an']
