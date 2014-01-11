{-------------------------------------------------------------------------------

 Computes BOW similarity scores over a list of word pairs

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import qualified DSem.VectorSpaceModel.BowCached as Bow
import qualified Data.Text as T
import Text.Printf
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.IO
import System.Environment
import System.Exit
import Data.Maybe

-- retains only the POS (first character after '_')
parseWord :: String -> String
parseWord w = case break (=='_') w of
  (l,_:p:_) -> l ++ "_" ++ [p]
  (l,_)     -> l

main = do
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: bow-pairs-sim <bow matrix> <list of word pairs>"
    exitFailure
  m  <- Bow.readMatrix (args!!0)
  ps <- map (parse . reverse . words) . lines <$> readFile (args!!1)
  Bow.runModelIO m $ do
    Bow.setCacheSize 100
    forM_ ps $ \(w1,w2) -> do
      s <- fromMaybe (-1) <$> Bow.targetCosine (T.pack w1) (T.pack w2)
      liftIO . putStrLn $ printf "%s\t%s\t%f" w1 w2 s
      -- add: vector norms + overlapping dimensions
  where parse (w2:w1:_) = (parseWord w1,parseWord w2)
        parse _         = error "no parse"

