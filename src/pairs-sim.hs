{-------------------------------------------------------------------------------

 Computes similarity scores over a list of word pairs

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import Control.Monad
import System.Environment
import Data.Maybe
import DSem.VectModel
import qualified DSem.VectModel.SparseBoW as BoW
import qualified DSem.VectModel.DepModel as Dep
import qualified Data.ByteString.UTF8 as B
import Text.Printf

-- TODO: add different similarity measures

main = do
  args <- getArgs
  if length args < 3
    then putStrLn "Usage: pairs-sim --[dep|bow] <distributional-model> <list-of-pairs>"
    else do
      bow <- BoW.readModel (args!!1)
      dep <- Dep.readModel (args!!1)
      let sim = if args!!0 == "--dep" 
                  then targetCosine dep else targetCosine bow
      ps <- (map (parse . reverse . words) . lines) `liftM` readFile (args!!2)
      putStr . unlines $ map (\(w1,w2) -> printf "%s\t%s\t%f" w1 w2 
        (fromMaybe (-1) $ sim (B.fromString w1) (B.fromString w2))) ps
  where parse (w2:w1:_) = (w1,w2)
        parse _         = error "no parse"

