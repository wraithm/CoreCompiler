module GraphReduction.Machine where

import Data.List (mapAccumL)

import Core.AST
import Core.Prelude
import Util.Heap

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
incStats s = s { stats = stats s + 1 }

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
    | Alloc Int
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
gmFinal = null . code

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
dispatch (Alloc n) = allocNodes n

unwind :: GmState -> GmState
unwind s = newState (hLookup h a)
  where
    h = heap s
    st = stack s
    (a:as) = st
    newState (NNum _) = s
    newState (NApp a1 _) = putCode [Unwind] (putStack (a1:a:as) s)
    newState (NInd a1) = putCode [Unwind] (putStack (a1:as) s)
    newState (NGlobal n c)
        | length as < n = error "Unwinding stack with too few arguments."
        | otherwise = putCode c (putStack rearranged s)
          where
            rearranged = take n as' ++ drop n st
            as' = map (getArg . hLookup h) as
            getArg (NApp _ a2) = a2

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
    a = as !! n

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

allocNodes :: Int -> GmState -> GmState
allocNodes n s = s { stack = newaddrs ++ stack s, heap = hp }
  where
    (hp, newaddrs) = allocNodes' n (heap s) 
    allocNodes' 0 h = (h, [])
    allocNodes' n h = (h'', a:as)
      where
        (h', as) = allocNodes' (n - 1) h
        (h'', a) = alloc h' (NInd hNull)
