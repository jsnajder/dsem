{-------------------------------------------------------------------------------

 DSem.Vector
 Vector interface

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module DSem.Vector (
  Vector (..),
  Weight,
  norm1,
  norm2,
  scale,
  normalize,
  sum,  
  linComb,
  VectorSim,
  cosine,
  centroid,
  dimShared) where  

import Data.List hiding (insert,sum,zipWith,map)
import Prelude hiding (zipWith,sum,map)
import qualified Data.List as L (sum,map)
import Data.Word (Word64)

------------------------------------------------------------------------------

type Weight = Double

class Vector v where
  empty          :: v
  -- number of stored weights
  size           :: v -> Word64
  -- combines vectors along common dimensions
  zipWith        :: (Weight -> Weight -> Weight) -> v -> v -> v
  -- maps function over weights
  map            :: (Weight -> Weight) -> v -> v
  -- vector addition
  add            :: v -> v -> v
  -- piecewise (componentwise) multiplication
  pmul           :: v -> v -> v
  -- dot product
  dot            :: v -> v -> Weight
  -- number of non-zero dimensions
  nonzeroes      :: v -> Word64
  -- number of non-zero dimensions
  nonzeroWeights :: v -> [Weight]
  -- from/to list conversions
  fromList       :: [Weight] -> v
  toList         :: v -> [Weight]
  toAssocList    :: v -> [(Word64,Weight)]
  fromAssocList  :: [(Word64,Weight)] -> v

  -- default implementations:

  add            = zipWith (+)
  pmul           = zipWith (*)
  dot v1 v2      = L.sum . nonzeroWeights $ pmul v1 v2
  nonzeroWeights = filter (/=0) . toList
  nonzeroes      = fromIntegral . length . nonzeroWeights

-- L1-norm
norm1 :: Vector v => v -> Weight
norm1 = L.sum . L.map abs . toList

-- L2-norm
norm2 :: Vector v => v -> Weight
norm2 v = sqrt $ v `dot` v

scale :: Vector v => Weight -> v -> v
scale w v = map (*w) v

normalize :: Vector v => v -> v
normalize v = scale (1 / norm2 v) v

sum :: Vector v => [v] -> v
sum [] = empty
sum vs = foldl1' add vs

linComb :: Vector v => [(Weight,v)] -> v
linComb = sum . L.map (uncurry scale)

type VectorSim v = v -> v -> Double 

cosine :: Vector v => VectorSim v
cosine v1 v2 
  | n1==0 || n2==0 = 0
  | otherwise      = v1 `dot` v2 / (n1 * n2)
  where n1 = norm2 v1
        n2 = norm2 v2

centroid :: Vector v => [v] -> v
centroid vs = scale (1 / (realToFrac $ length vs)) $ sum vs

dimShared :: Vector v => v -> v -> Word64
dimShared v = nonzeroes . pmul v

