-- Size-bounded strict map
-- (c) 2013 Jan Snajder

module Data.BoundedMap where

import qualified Data.Map as M
import qualified Data.Dequeue as Q
import Data.Maybe
import Debug.Trace

data BoundedMap k a = BM {
  dict  :: M.Map k a,
  elems :: Q.BankersDequeue k,
  bound :: Int } 
  deriving (Eq,Show)

empty :: Int -> BoundedMap k a
empty n = setBound n $ BM M.empty Q.empty 1

setBound :: Int -> BoundedMap k a -> BoundedMap k a
setBound 0 _ = error "Map size must be greater than zero"
setBound n m = m { bound = n }

-- don't know why, but without seqs stack space increases
insert :: (Ord k, Show k) => k -> a -> BoundedMap k a -> BoundedMap k a
insert k x (BM m q b) = m1 `seq` q1 `seq` BM (M.insert k x m1) (Q.pushFront q1 k) b
  where (m1,q1) | M.size m == b = let (Just y, q') = Q.popBack q
                                  in (M.delete y m, q')
                | otherwise      = (m,q)
  
lookup :: Ord k => k -> BoundedMap k a -> Maybe a
lookup k (BM m _ _) = M.lookup k m

size :: BoundedMap k a -> Int
size (BM m _ _) = M.size m

