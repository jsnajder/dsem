-- Size-bounded strict map
-- (c) 2013 Jan Snajder

module Data.BoundedMap where

import qualified Data.Map.Strict as MS
import qualified Data.Dequeue as Q
import Data.Maybe

data BoundedMap k a = BM {
  dict  :: MS.Map k a,
  elems :: Q.BankersDequeue k,
  cache :: Int } 
  deriving (Eq,Show)

empty :: BoundedMap k a
empty = BM MS.empty Q.empty 1

setSize :: Int -> BoundedMap k a -> BoundedMap k a
setSize 0 _ = error "Map size must be greater than zero"
setSize n m = m { cache = n }

insert :: Ord k => k -> a -> BoundedMap k a -> BoundedMap k a
insert k x (BM m q c) = BM (MS.insert k x m1) (Q.pushFront q1 k) c
  where (m1,q1) | MS.size m == c = let (Just y,q') = Q.popBack q 
                                   in (MS.delete y m,q')
                | otherwise      = (m,q)
  
lookup :: Ord k => k -> BoundedMap k a -> Maybe a
lookup k (BM m _ _) = MS.lookup k m

