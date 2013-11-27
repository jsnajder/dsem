{-------------------------------------------------------------------------------

 DSem.VectModel
 Interface

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module DSem.VectModel where

import Control.Monad
import qualified Data.Map as M
import qualified DSem.Vector as V
import DSem.Vector (Vector)
import Control.Monad.State
import Control.Monad.Reader

{-
class (Vector v, Monad m) => MVectModel a m t v where
  getVector :: a -> t -> m (Maybe v)
  getDim    :: a -> m (Int,Int)
--  toList   :: m [(t,v)]

instance Vector v => MVectModel (CachedVectModel v t) IO t v

data CachedVectModel v t
-}
--

class (Vector v, Monad m) => VectModelM m t c v where
  getVector   :: t -> m (Maybe v)
  getDim      :: m (Int,Int)
  getTargets  :: m [t]
  getContexts :: m [c]

type ModelIO a   = StateT a IO
type ModelPure a = Reader a

runPure :: ModelPure a b -> a -> b
runPure = runReader

runIO :: ModelIO a b -> a -> IO b
runIO = evalStateT



--

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

