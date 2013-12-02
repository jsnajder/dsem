{-------------------------------------------------------------------------------

 DSem.Vector
 Vector interface

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module DSem.Vector where

import Data.List hiding (insert,sum,zipWith,map)
import Prelude hiding (zipWith,sum,map)
import qualified Data.List (sum,map)
import Control.Monad
import Data.Maybe
import Data.Word
import qualified Data.Map as M

------------------------------------------------------------------------------

type Weight = Double

class Eq v => Vector v where
  empty         :: v
  -- number of stored weights
  size          :: v -> Int
  -- combines vectors along common dimensions
  zipWith       :: (Weight -> Weight -> Weight) -> v -> v -> v
  -- maps function over weights
  map           :: (Weight -> Weight) -> v -> v
  -- vector addition
  add           :: v -> v -> v
  -- piecewise (componentwise) multiplication
  pmul          :: v -> v -> v
  -- dot product
  dot           :: v -> v -> Weight
  -- non-zero weights
  nonzeros      :: v -> [Weight]
  -- from/to list conversions
  fromList      :: [Weight] -> v
  toList        :: v -> [Weight]
  toAssocList   :: v -> [(Int,Weight)]
  fromAssocList :: [(Int,Weight)] -> v

  -- default implementations:

  add       = zipWith (+)
  pmul      = zipWith (*)
  dot v1 v2 = Data.List.sum . nonzeros $ pmul v1 v2
  nonzeros  = filter (>0) . toList

norm :: Vector v => v -> Weight
norm v = sqrt $ v `dot` v

scale :: Vector v => Weight -> v -> v
scale w v = map (*w) v

normalize :: Vector v => v -> v
normalize v = scale (1 / norm v) v

sum :: Vector v => [v] -> v
sum [] = empty
sum vs = foldl1' add vs

linComb :: Vector v => [(Weight,v)] -> v
linComb = sum . Data.List.map (uncurry scale)

type Sim v = v -> v -> Double 

cosine :: Vector v => Sim v
cosine v1 v2 
  | n1==0 || n2==0 = 0
  | otherwise      = v1 `dot` v2 / (n1 * n2)
  where n1 = norm v1
        n2 = norm v2

centroid :: Vector v => [v] -> v
centroid vs = scale (1 / (realToFrac $ length vs)) $ sum vs

