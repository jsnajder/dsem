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
import System.CPUTime (getCPUTime)

-- retains only the POS (first character after '_')
parseWord :: String -> String
parseWord w = case break (=='_') w of
  (l,_:p:_) -> l ++ "_" ++ [p]
  (l,_)     -> l

parsePairs :: String -> [(String,String)]
parsePairs = map (parse . words) . lines
  where parse (w1:w2:_) = (parseWord w1,parseWord w2)
        parse _         = error "no parse"

rankSim :: Int -> String -> String -> Bow.ModelM (Double,Double)
rankSim n w1 w2 = do
  s1 <- rankSim' n V.cosine (T.pack w1) (T.pack w2)
  s2 <- rankSim' n V.cosine (T.pack w2) (T.pack w1)
  return (s1,s2)

main = do
  args <- getArgs
  when (length args < 3) $ do
    putStrLn "Usage: bowRankSim <bow matrix> <num of neighbors> <list of word pairs>"
    exitFailure
  let n = read (args!!1) 
  m  <- Bow.readMatrix (args!!0)
  ps <- parsePairs <$> readFile (args!!2)
  hSetBuffering stdout LineBuffering
  putStrLn "word_1\tword_2\trank-sim-LR\trank-sim-RL"
  t1 <- liftIO $ getCPUTime
  Bow.runModelIO m $ do
    forM_ ps $ \(w1,w2) -> do
      (s1,s2) <- rankSim n w1 w2
      liftIO . putStrLn $ printf "%s\t%s\t%.3f\t%.3f" w1 w2 s1 s2
  t2 <- liftIO $ getCPUTime
  let t = (realToFrac (t2 - t1) / realToFrac (10^12 :: Int)) :: Double
  liftIO . hPutStrLn stderr $ 
    printf "Took %.3f seconds (%.3f seconds per pair)" 
      t (t / realToFrac (length (ps)))

