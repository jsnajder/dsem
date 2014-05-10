{-------------------------------------------------------------------------------

 Computes BOW neighborhood rankes similarity scores for a list of word pairs.

 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import qualified DSem.VectorSpace.BowCached as Bow
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

-- retains only the POS (first character after '_')
parseWord :: String -> String
parseWord w = case break (=='_') w of
  (l,_:p:_) -> l ++ "_" ++ [p]
  (l,_)     -> l

parsePairs :: String -> [(String,String)]
parsePairs = map (parse . reverse . words) . lines
  where parse (w2:w1:_) = (parseWord w1,parseWord w2)
        parse _         = error "no parse"

rankSim :: String -> String -> Bow.ModelM (Double,Double)
rankSim w1 w2 = do
  s1 <- rankSim' V.cosine (T.pack w1) (T.pack w2)
  s2 <- rankSim' V.cosine (T.pack w2) (T.pack w1)
  return (s1,s2)

main = do
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: bowRankSim <bow matrix> <list of word pairs>"
    exitFailure
  m  <- Bow.readModel (args!!0) (args!!1)
  ps <- parsePairs <$> readFile (args!!2)
  putStrLn "word_1\tword_2\trank-sim1\trank-sim2"
  Bow.runModelIO m $ do
    forM_ ps $ \(w1,w2) -> do
      (s1,s2) <- rankSim w1 w2
      liftIO . putStrLn $ printf "%s\t%s\t%.3f\t%.3f" w1 w2 s1 s2

