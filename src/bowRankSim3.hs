{-------------------------------------------------------------------------------

 Computes BOW neighborhood rankes similarity scores for a list of word pairs.

 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-- TMP version: reading in all neighborhood vectors and passing them to rankSim
   as a list of vetors instead of a list of targets (speed up because we don't 
   need to read form file every time)
   TODO: implement this transparently via cache!!!

-------------------------------------------------------------------------------}

import qualified DSem.VectorSpace.Bow as Bow
import DSem.VectorSpace.Similarity (rankSimOn2')
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

type Word = T.Text

-- retains only the POS (first character after '_')
parseWord :: String -> String
parseWord w = case break (=='_') w of
  (l,_:p:_) -> l ++ "_" ++ [p]
  (l,_)     -> l

parsePairs :: String -> [(String,String)]
parsePairs = map (parse . words) . lines
  where parse (w1:w2:_) = (parseWord w1,parseWord w2)
        parse _         = error "no parse"

rankSim :: [Bow.Vect] -> Int -> Word -> Word -> Bow.ModelM (Double,Double)
rankSim ns n w1 w2 = do
  s1 <- rankSimOn2' ns n V.cosine w1 w2
  s2 <- rankSimOn2' ns n V.cosine w2 w1
  return (s1,s2)

main = do
  args <- getArgs
  when (length args < 4) $ do
    putStrLn "Usage: bowRankSim <bow matrix> <list of neighbors> <num of neighbors> <list of word pairs>"
    exitFailure
  let n = read (args!!2) 
  m  <- Bow.readMatrix (args!!0)
  ns' <- map (T.pack . parseWord) . lines <$> readFile (args!!1)
  ps <- parsePairs <$> readFile (args!!3)
  hSetBuffering stdout LineBuffering
  putStrLn "word_1\tword_2\trank-sim-LR\trank-sim-RL"
  t1 <- liftIO $ getCPUTime
  Bow.runModelIO m $ do
    ns <- catMaybes <$> forM ns' Bow.getVector
    forM_ ps $ \(w1,w2) -> do
      (s1,s2) <- rankSim ns n (T.pack w1) (T.pack w2)
      liftIO . putStrLn $ printf "%s\t%s\t%.3f\t%.3f" w1 w2 s1 s2
  t2 <- liftIO $ getCPUTime
  let t = (realToFrac (t2 - t1) / realToFrac (10^12 :: Int)) :: Double
  liftIO . hPutStrLn stderr $ 
    printf "Took %.3f seconds (%.3f seconds per pair)" 
      t (t / realToFrac (length (ps)))

