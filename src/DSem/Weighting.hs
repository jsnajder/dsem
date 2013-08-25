
{-------------------------------------------------------------------------------

 DSem.Weighting

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>
 
 TODO: Add PMI and LL weighting schemes

-------------------------------------------------------------------------------}

module DSem.Weighting where

import qualified Data.Map as M

-- pointwise mutual information
pmi :: Double -> Double -> Double -> Double -> Double
pmi n fx fy fxy 
  | n * fx * fy * fxy == 0 = 0
  | otherwise = log fxy + log n - log fx - log fy

-- local mutual information
lmi :: Double -> Double -> Double -> Double -> Double
lmi n fx fy fxy = fxy * pmi n fx fy fxy
