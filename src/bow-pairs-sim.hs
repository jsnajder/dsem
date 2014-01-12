{-------------------------------------------------------------------------------

 Computes BOW similarity scores over a list of word pairs

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import qualified DSem.VectorSpaceModel.BowCached as Bow
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

-- retains only the POS (first character after '_')
parseWord :: String -> String
parseWord w = case break (=='_') w of
  (l,_:p:_) -> l ++ "_" ++ [p]
  (l,_)     -> l

similarity :: String -> String -> Bow.ModelM (Double,Double,Double,Int)
similarity w1 w2 = do
  v1 <- Bow.getVector $ T.pack w1
  v2 <- Bow.getVector $ T.pack w2
  return $ case (v1,v2) of
    (Just v1,Just v2) -> 
      (V.cosine v1 v2, V.norm v1, V.norm v2, V.dimShared v1 v2)
    _ -> (-1,-1,-1,-1)

main = do
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: bow-pairs-sim <bow matrix> <list of word pairs>"
    exitFailure
  m  <- Bow.readMatrix (args!!0)
  ps <- map (parse . reverse . words) . lines <$> readFile (args!!1)
  putStrLn "word_1\tword_2\tcosine\tnorm_1\tnorm_2\tdim_shared"
  Bow.runModelIO m $ do
    Bow.setCacheSize 100
    forM_ ps $ \(w1,w2) -> do
      (s,n1,n2,d) <- similarity w1 w2
      liftIO . putStrLn $ printf "%s\t%s\t%f\t%f\t%f\t%d" w1 w2 s n1 n2 d
  where parse (w2:w1:_) = (parseWord w1,parseWord w2)
        parse _         = error "no parse"

