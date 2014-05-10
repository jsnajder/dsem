{-------------------------------------------------------------------------------

 DSem.VectorSpace
 Vector space model interface

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module DSem.VectorSpace (
  module DSem.Vector,
  Model (..),
  ModelIO,
  ModelPure,
  runModelIO,
  runModelPure,
  existsTarget) where

import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified DSem.Vector as V
import DSem.Vector (Vector)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Maybe

class (Vector v, Monad m) => Model m t c v | m -> v, m -> t, m -> c where
  getVector   :: t -> m (Maybe v)
  getDim      :: m (Int,Int)
  getTargets  :: m [t]
  getContexts :: m [c]

type ModelIO a   = StateT a IO
type ModelPure a = Reader a

runModelPure :: a -> ModelPure a b -> b
runModelPure = flip runReader

runModelIO :: a -> ModelIO a b -> IO b
runModelIO = flip evalStateT

existsTarget :: Model m t c v => t -> m Bool
existsTarget t = isJust `liftM` getVector t

{-
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

-}
