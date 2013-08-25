{-------------------------------------------------------------------------------

 DSem.VectModel
 Interface

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module DSem.VectModel where

import Control.Monad
import qualified Data.Map as M
import qualified DSem.Vector as V

class V.Vector v => VectModel m t v | m -> t, m -> v where
  vect     :: m -> t -> Maybe v
  dims     :: m -> (Int,Int)
  toList   :: m -> [(t,v)]
--  fromList :: [(t,v)] -> m

type TargetSim t = t -> t -> Maybe Double

targetSim :: VectModel m t v => V.Sim v -> m -> TargetSim t
targetSim sim m t1 t2 = liftM2 sim (vect m t1) (vect m t2)

targetCosine :: VectModel m t v => m -> t -> t -> Maybe Double
targetCosine = targetSim V.cosine

targetMarginals :: (VectModel m t v, Ord t) => m -> M.Map t Double
targetMarginals m = 
  M.fromList [ (t, sum $ V.toList v) | (t,v) <- toList m]

contextMarginals :: VectModel m t v => m -> v
contextMarginals = V.sum . map snd . toList

-- todo: conflate into a single function, with LMI | PMI ...
{-
lmiWeighting :: (Ord t, DModel m t v) => m -> m
lmiWeighting m = fromList . map f $ toList m
  where tm  = targetMarginals m
        cm  = contextMarginals m
        n   = sum $ V.toList cm
        f (t,v) = (t,vzip (\fx fxy -> 
          lmi n fx (M.findWithDefault 0 t tm) fxy) cm v)
-}

lmi :: Double -> Double -> Double -> Double -> Double
lmi n fx fy fxy 
  | n * fx * fy * fxy == 0 = 0
  | otherwise = fxy * (log fxy + log n - log fx - log fy) 

