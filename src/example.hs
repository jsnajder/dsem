import DSem.VectModel.Bow
import qualified DSem.VectModel.BowCached as Bow
import DSem.VectModel
import qualified Data.ByteString.UTF8 as B
import qualified DSem.Vector.SparseVector as V
import DSem.Vector
import Control.Monad
import Control.Monad.Trans
--import Control.Monad.Trans

main = do
  m <- Bow.readModel "../data/hrwac-sample.bow.contexts" 
                    "../data/hrwac-sample.bow.matrix"
  c <- Bow.runModelIO m $ do
            Bow.setCacheSize 4
            x <- Bow.loadAllTargets
            liftIO $ print x
            v1 <- getVector $ B.fromString "jesen"
            v2 <- getVector $ B.fromString "kino"
            v3 <- getVector $ B.fromString "kist"
            v4 <- getVector $ B.fromString "jesen"
            v1 <- getVector $ B.fromString "kompletan"
            v4 <- getVector $ B.fromString "jesen"
            v4 <- getVector $ B.fromString "kino"
            v3 <- getVector $ B.fromString "agnosticiza"
            st <- Bow.getCacheStats
            liftIO $ print st
            dim <- getDim
            liftIO $ print dim
            s <- targetCosine (B.fromString "kompletan") (B.fromString "kino")
            liftIO $ print s
            return $ liftM2 cosine v1 v4
             
  print c
  return ()

{-
main = do
  m <- BC.readModel "../data/hrwac-sample.bow"
  let c = runModel m $ do
            v1 <- getVector (B.fromString "jesen")
            v2 <- getVector (B.fromString "kino")
            return $ liftM2 cosine v1 v2
  print c
  return ()
-}

