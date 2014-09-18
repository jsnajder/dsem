{-------------------------------------------------------------------------------

 DSem.VectorSpace
 Vector space model interface

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module DSem.VectorSpace
  ( existsTarget
  , module DSem.Vector 
  , getTargetVector
  , Model (..) 
  , ModelIO 
  , ModelPure 
  , runModelIO 
  , runModelIO2 
  , runModelPure
  , Targetable (..) ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Maybe
import qualified DSem.Vector
import DSem.Vector (Vector)

class (Monad m, Vector v) => 
  Model m t c v | m -> v, m -> t, m -> c where
  getVector   :: t -> m (Maybe v)
  getDim      :: m (Int,Int)
  getTargets  :: m [t]
  getContexts :: m [c] 

class Targetable a t where
  toTarget   :: a -> t
  fromTarget :: t -> a

getTargetVector :: (Model m t c v, Targetable a t) => a -> m (Maybe v)
getTargetVector = getVector . toTarget

type ModelIO a   = StateT a IO
type ModelPure a = Reader a

runModelPure :: a -> ModelPure a b -> b
runModelPure = flip runReader

runModelIO :: a -> ModelIO a b -> IO b
runModelIO = flip evalStateT

runModelIO2 :: a -> ReaderT a IO b -> IO b
runModelIO2 = flip runReaderT

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
