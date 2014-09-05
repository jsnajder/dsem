{-------------------------------------------------------------------------------

 Computes BOW neighborhood rankes similarity scores for a list of word pairs.

 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import qualified DSem.VectorSpace.BowCached2 as Bow
import DSem.VectorSpace.Similarity (rankSim')
import qualified DSem.Vector as V
import qualified Data.Text as T
import Text.Printf
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.IO
import System.Environment
import System.Exit
import Data.Maybe
import Data.List
import Data.Ord

main = do
  args <- getArgs
  when (length args < 1) $ do
    putStrLn "Usage: test <bow matrix>"
    exitFailure
  m  <- Bow.readMatrix (args!!0)
  Bow.runModelIO2 m $ do
    ts <- Bow.getTargets
    liftIO . putStrLn . printf "%d targets read" $ length ts
    let t = last ts
    liftIO . putStrLn $ printf "Last target is: \"%s\"" (T.unpack t)
    Just v <- Bow.getVector t
    liftIO $ do
      putStrLn $ printf "Norm of \"%s\" is %f" (T.unpack t) (V.norm2 v)
      putStr $ "Press any key to continue..."
      getChar
    forM_ ts $ \t -> do
      Just v <- Bow.getVector t
      liftIO . putStrLn $ printf "Norm of \"%s\" is %f" (T.unpack t) (V.norm2 v)

