{-------------------------------------------------------------------------------

 DSem.Vector.SparseVector
 Sparse distributional vector representation

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveFoldable, InstanceSigs #-} --TMP

module DSem.Vector.SparseVector
  (module DSem.Vector,
   SparseVector) where

import DSem.Vector
import qualified Data.Map.Strict as M
import Data.Word
import Data.Maybe

type Index = Int
newtype SparseVector = SV (M.Map Index Weight)
  deriving (Eq,Show,Read,Ord)

instance Vector SparseVector where

  empty = SV M.empty

  size (SV v) = M.size v

  zipWith f (SV v1) (SV v2) =
    SV $ M.filter (/=0) $ 
    zipAligned (\w1 w2 -> f (fromMaybe 0 w1) (fromMaybe 0 w2)) v1 v2
  
  map f (SV v) = SV (M.map f v)
        
  add (SV v1) (SV v2) = SV $ M.unionWith (+) v1 v2

  pmul (SV v1) (SV v2) = SV $ M.intersectionWith (*) v1 v2

  nonzeros (SV v) = M.elems v

  toList (SV v) = extendVector $ M.toAscList v

  fromList = SV . M.fromList . zip [0..]

  toAssocList (SV v) = M.toList v
  
  fromAssocList = SV . M.fromList

extendVector :: [(Index,Weight)] -> [Weight]
extendVector = pad 0
  where pad _ []  = []
        pad j zs@((i,w):xs) | j < i     = 0 : pad (j+1) zs
                            | otherwise = w : pad (i+1) xs

-- zips two maps by applying combination function on keys that match
-- for the rest of entries, combination function is applied only on one of the
-- two maps
zipAligned :: Ord k => 
  (Maybe a -> Maybe b -> c) -> M.Map k a -> M.Map k b -> M.Map k c
zipAligned f m1 m2 = M.unions $
  [M.intersectionWith (\x y -> f (Just x) (Just y))  m1 m2, 
   M.map (\x -> f (Just x) Nothing) (M.difference m1 m2), 
   M.map (\x -> f Nothing (Just x)) (M.difference m2 m1)]
-- traversing a sorted list would probably be faster

