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

targetSim :: Model m t c v => VectorSim v -> t -> t -> m (Maybe Double)
targetSim sm t1 t2 = do 
  v1 <- getVector t1
  v2 <- getVector t2
  return $ liftM2 sm v1 v2

cosineSim :: Model m t c v => t -> t -> m (Maybe Double)
cosineSim = targetSim cosine

cosineSim' :: Model m t c v => t -> t -> m Double
cosineSim' t1 t2 = fromMaybe 0 `liftM` targetSim cosine t1 t2

rankSim :: Model m t c v => VectorSim v -> t -> t -> m (Maybe Double)
rankSim sm t1 t2 = do
  ts <- getTargets
  s  <- targetSim sm t1 t2
  case s of
    Just s -> do ss <- map fromJust `liftM` mapM (targetSim sm t1) ts
                 return . Just $ realToFrac (rankIn s ss) 
                                 / realToFrac (length ts)
    _      -> return Nothing

rankSim' :: Model m t c v => VectorSim v -> t -> t -> m Double
rankSim' sm t1 t2 = fromMaybe 0 `liftM` rankSim sm t1 t2

rankIn :: Double -> [Double] -> Int
rankIn x = (+1) . length . filter (<x) 

