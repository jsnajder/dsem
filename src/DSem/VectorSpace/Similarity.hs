{-------------------------------------------------------------------------------

 DSem.VectorSpace
 Vector space model interface

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module DSem.VectorSpace.Similarity where

import DSem.VectorSpace
import Control.Monad
import DSem.Vector (Vector,VectorSim,cosine)
import Data.Maybe

targetSim :: (Model m t c v, Targetable a t) => 
  VectorSim v -> a -> a -> m (Maybe Double)
targetSim sm t1 t2 = do 
  v1 <- getTargetVector t1
  v2 <- getTargetVector t2
  return $ liftM2 sm v1 v2

cosineSim :: (Model m t c v, Targetable a t) => a -> a -> m (Maybe Double)
cosineSim = targetSim cosine

cosineSim' :: (Model m t c v, Targetable a t) => a -> a -> m Double
cosineSim' t1 t2 = fromMaybe 0 `liftM` targetSim cosine t1 t2

{-
 - TMP OFF  2014-09-18
 
-- Computes similarity as @1 - rank/n@
-- where @n@ is the number of neighbors to consider.
-- TODO: switch to Targetable instance
rankSim :: (Model m t c v, Targetable a t) 
  => Int -> VectorSim v -> a -> a -> m (Maybe Double)
rankSim n sm t1 t2 = do
  ts <- getTargets
  rankSimOn ts n sm t1 t2

rankSim' :: (Model m t c v, Targetable a t) 
  => Int -> VectorSim v -> a -> a -> m Double
rankSim' n sm t1 t2 = fromMaybe 0 `liftM` rankSim n sm t1 t2

rankSimOn :: (Model m t c v, Targetable a t)
  => [t] -> Int -> VectorSim v -> a -> a -> m (Maybe Double)
rankSimOn ts n sm t1 t2 = do
  s <- targetSim sm t1 t2
  case s of
    Just s -> do ss <- map fromJust `liftM` mapM (targetSim sm t1) ts
                 let n' = min n (length ts)
                 return . Just $ 1 - realToFrac (rank n' s ss) / realToFrac n'
    _      -> return Nothing
  where rank n x = min n . length . filter (>x) 

rankSimOn' :: Model m t c v => [t] -> Int -> VectorSim v -> t -> t -> m Double
rankSimOn' ts n sm t1 t2 = fromMaybe 0 `liftM` rankSimOn ts n sm t1 t2

-- TMP:

rankSimOn2 :: Model m t c v => 
  [v] -> Int -> VectorSim v -> t -> t -> m (Maybe Double)
rankSimOn2 vs n sm t1 t2 = do
  v1 <- getVector t1
  v2 <- getVector t2
  case liftM2 sm v1 v2 of
    Just s -> do let ss = map (sm $ fromJust v1) vs
                     n' = min n (length vs)
                 return . Just $ 1 - realToFrac (rank n' s ss) / realToFrac n'
    _      -> return Nothing
  where rank n x = min n . length . filter (>x) 

rankSimOn2' :: Model m t c v => [v] -> Int -> VectorSim v -> t -> t -> m Double
rankSimOn2' ts n sm t1 t2 = fromMaybe 0 `liftM` rankSimOn2 ts n sm t1 t2
-}
