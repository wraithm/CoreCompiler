module Util.Heap where

import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)

type Size = Int
type Addr = Int

data Heap a = Heap 
    { size    :: Size 
    , unused  :: [Addr] 
    , objects :: M.Map Addr a }

instance Show a => Show (Heap a) where
    show h = "Heap " ++ show (size h) ++ " " ++ show (objects h)

emptyHeap = Heap 0 [1..] M.empty

alloc :: Heap a -> a -> (Heap a, Addr)
alloc (Heap size (next:free) objs) n = (Heap (size + 1) free objs', next)
    where objs' = M.insert next n objs

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate heap a n = heap { objects = M.insert a n (objects heap) }

free :: Heap a -> Addr -> Heap a
free (Heap s f objs) a = Heap (s - 1) (a:f) (M.delete a objs)

hLookup :: Heap a -> Addr -> a
hLookup heap addr = fromMaybe err $ M.lookup addr (objects heap) 
    where err = error $ "Could not find address " ++ show addr ++ " in the heap."
