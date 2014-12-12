{-------------------------------------------------------------------------------

 Computes BOW similarity scores over a list of MWE pairs and
 reduces rach MWE pair to a word pair that maximizes cosine similarity.

 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE DoAndIfThenElse #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Data.Word (Word64)
import qualified Data.Text as T
import qualified DSem.Vector as V
import DSem.VectorSpace.Bow as Bow
import DSem.VectorSpace.Similarity (cosineSim')
import System.Console.ParseArgs
import System.Environment
import System.Exit
import System.IO
import Text.Printf

mwe2word :: String -> String -> String -> BowM (String, String)
mwe2word sep p1 p2 = 
  fst . maximumBy (compare `on` snd) <$>
  sequence [sim w1 w2 | w1 <- split p1, w2 <- split p2]
  where split = splitOn sep
        sim w1 w2 = do s <- cosineSim' w1 w2
                       return ((w1,w2),s)

arg =
  [ Arg 0 (Just 's') (Just "word-separator") 
    (argDataDefaulted "string" ArgtypeString "-")
    "separator for words in a phrase (default=\"-\")"
  , Arg 1 Nothing Nothing  (argDataRequired "bow" ArgtypeString)
    "BoW matrix filename"
  , Arg 2 Nothing Nothing  (argDataRequired "pairs" ArgtypeString)
    "list of word pairs" ]

main = do
  args <- parseArgsIO ArgsComplete arg
  let matrix = fromJust $ getArg args 1
      pairs  = fromJust $ getArg args 2
      sep    = fromJust $ getArg args 0 
  ps <- let parse (w1:w2:_) = (w1, w2)
            parse _         = error "no parse"
        in map (parse . words) . lines <$> readFile pairs
  m <- Bow.readMatrix matrix
  Bow.runModelIO m $ do
    forM_ ps $ \(p1,p2) -> do
      (w1,w2) <- mwe2word sep p1 p2
      liftIO . putStrLn $ printf "%s %s" w1 w2
