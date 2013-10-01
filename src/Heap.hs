module Heap where

import qualified Data.Map.Strict as M

type Size = Int
type Addr = Int

data Heap a = Heap 
    { size    :: Size 
    , unused  :: [Addr] 
    , objects :: M.Map Addr a
    } deriving Show

emptyHeap = Heap 0 [1..] M.empty

alloc :: Heap a -> a -> (Heap a, Addr)
alloc (Heap size (next:free) objs) n = (Heap (size + 1) free objs', next)
    where objs' = M.insert next n objs

update :: Heap a -> Addr -> a -> Heap a
update heap a n = heap { objects = M.insert a n (objects heap) }

free :: Heap a -> Addr -> Heap a
free (Heap s f objs) a = Heap (s - 1) (a:f) (M.delete a objs)

