
module Data.CacheMap where

import qualified Data.Map.Strict as MS
import qualified Data.Dequeue as Q
import Data.Maybe

data CacheMap k a = CM {
  dict  :: MS.Map k a,
  elems :: Q.BankersDequeue k,
  cache :: Int } 
  deriving (Eq,Show)

empty :: CacheMap k a
empty = CM MS.empty Q.empty 1

setCache :: Int -> CacheMap k a -> CacheMap k a
setCache 0 _ = error "Cache size must be greater than zero"
setCache n m = m { cache = n }

insert :: Ord k => k -> a -> CacheMap k a -> CacheMap k a
insert k x (CM m q c) = CM (MS.insert k x m1) (Q.pushFront q1 k) c
  where (m1,q1) | MS.size m == c = let (Just y,q') = Q.popBack q 
                                   in (MS.delete y m,q')
                | otherwise      = (m,q)
  
lookup :: Ord k => k -> CacheMap k a -> Maybe a
lookup k cm@(CM m _ _) = MS.lookup k m

