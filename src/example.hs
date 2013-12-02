import DSem.VectModel.Bow
import qualified DSem.VectModel.BowCached as BC
import DSem.VectModel
import qualified Data.ByteString.UTF8 as B
import qualified DSem.Vector.SparseVector as V
import DSem.Vector
import Control.Monad
--import Control.Monad.Trans

main = do
  m <- BC.readModel "../data/hrwac-sample.bow"
  c <- BC.runModel m $ do
            BC.setCache 3
            v1 <- getVector $ B.fromString "jesen"
            v2 <- getVector $ B.fromString "kino"
            v3 <- getVector $ B.fromString "kist"
            v4 <- getVector $ B.fromString "jesen"
            v4 <- getVector $ B.fromString "kompletan"
            v4 <- getVector $ B.fromString "jesen"
            v4 <- getVector $ B.fromString "kino"
            v4 <- getVector $ B.fromString "kompletan"
            return $ liftM2 cosine v1 v2
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

