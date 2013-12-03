import qualified DSem.VectModel.BowCached as Bow
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified DSem.Vector as V
import Control.Monad
import Control.Applicative
import Control.Monad.Trans

main = do
  m <- Bow.readModel "../data/hrwac-sample.bow.contexts" 
                    "../data/hrwac-sample.bow.matrix"
  ts <- T.lines <$> T.readFile "../data/hrwac-sample.bow.targets"
  Bow.runModelIO m $ do
    Bow.setCacheSize 100
    forM ts $ \t -> do
      v <- Bow.getVector t
      liftIO . print $ V.size <$> v
    st <- Bow.getCacheStats
    liftIO $ print st

--            x <- Bow.cacheAllTargets
--            liftIO $ print x
{-            v1 <- getVector $ B.fromString "jesen"
            v2 <- getVector $ B.fromString "kino"
            v3 <- getVector $ B.fromString "kist"
            v4 <- getVector $ B.fromString "jesen"
            v1 <- getVector $ B.fromString "kompletan"
            v4 <- getVector $ B.fromString "jesen"
            v4 <- getVector $ B.fromString "kino"
            v3 <- getVector $ B.fromString "agnosticiza"
            dim <- getDim
            liftIO $ print dim
-}
--            s <- targetCosine (B.fromString "kompletan") (B.fromString "kino")
--            liftIO $ print s

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

