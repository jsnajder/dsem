{-------------------------------------------------------------------------------

 DSem.Vector
 Vector interface

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module DSem.Vector 
  ( Vector (..)
  , Weight
  , norm1
  , norm2
  , scale
  , normalize
  , sum
  , linComb
  , VectorSim
  , cosine
  , centroid
  , dimShared
  , entropy
  , klDivergence
  , jsDivergence
  , jaccardIndex
  , toDistribution
  , nonzeroDims ) where  

import Data.List hiding (insert,sum,zipWith,map)
import Prelude hiding (zipWith,sum,map)
import qualified Data.List as L (sum,map)
import Data.Word (Word64)

------------------------------------------------------------------------------

type Weight = Double
type Dim    = Word64

class Vector v where
  empty          :: v
  -- number of stored weights
  size           :: v -> Dim
  -- combines vectors along common dimensions
  zipWith        :: (Weight -> Weight -> Weight) -> v -> v -> v
  -- maps function over weights ---> TODO: ill-defined: behaves differently for sparse and non sparse!
  map            :: (Weight -> Weight) -> v -> v
  -- vector addition
  add            :: v -> v -> v
  -- piecewise (componentwise) multiplication
  pmul           :: v -> v -> v
  -- dot product
  dot            :: v -> v -> Weight
  -- number of non-zero dimensions
  nonzeroes      :: v -> Dim 
  -- nonzero wieghts
  nonzeroWeights :: v -> [Weight]
  -- from/to list conversions
  fromList       :: [Weight] -> v
  toList         :: v -> [Weight]
  toAssocList    :: v -> [(Dim,Weight)]
  fromAssocList  :: [(Dim,Weight)] -> v

  -- default implementations:

  add            = zipWith (+)
  pmul           = zipWith (*)
  dot v1 v2      = L.sum . nonzeroWeights $ pmul v1 v2
  nonzeroWeights = filter (/=0) . toList
  nonzeroes      = fromIntegral . length . nonzeroWeights

nonzeroDims :: Vector v => v -> [Dim]
nonzeroDims = L.map fst . filter ((/=0) . snd) . toAssocList

-- L1-norm
norm1 :: Vector v => v -> Weight
norm1 = L.sum . L.map abs . nonzeroWeights

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

dimShared :: Vector v => v -> v -> Dim
dimShared v = nonzeroes . pmul v

entropy :: Vector v => v -> Double
entropy = negate. L.sum . nonzeroWeights . map (\p -> p * log p) . normalize

toDistribution :: Vector v => v -> v
toDistribution v = map (\w -> w / n) v
  where n = L.sum $ nonzeroWeights v

-- TODO: Move to VectorSpace.Similarity
klDivergence :: Vector v => v -> v -> Double
klDivergence v1 v2 = klDivergence' (toDistribution v1) (toDistribution v2)

-- Assumes vectors are distributions.
klDivergence' :: Vector v => v -> v -> Double
klDivergence' v1 v2 = 
  L.sum . nonzeroWeights $ 
  zipWith (\p q -> if p==0 then 0 else p * log (p / q)) v1 v2

jsDivergence :: Vector v => v -> v -> Double
jsDivergence v1 v2 = (klDivergence' p m + klDivergence' q m) / 2
  where p = toDistribution v1
        q = toDistribution v2
        m = scale (1/2) $ p `add` q 

jaccardIndex :: Vector v => v -> v -> Double
jaccardIndex v1 v2 = m / n 
  where m = L.sum . nonzeroWeights $ zipWith min v1 v2 
        n = L.sum . nonzeroWeights $ zipWith max v1 v2

