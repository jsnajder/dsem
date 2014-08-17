{-------------------------------------------------------------------------------

 Computes BOW similarity scores over a list of word pairs

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import qualified DSem.VectorSpace.BowCached2 as Bow
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
import Data.Word (Word64)

-- retains only the POS (first character after '_')
parseWord :: String -> String
parseWord w = case break (=='_') w of
  (l,_:p:_) -> l ++ "_" ++ [p]
  (l,_)     -> l

similarity :: String -> String -> Bow.ModelM (Double,Double,Double,Word64)
similarity w1 w2 = do
  v1 <- Bow.getVector $ T.pack w1
  v2 <- Bow.getVector $ T.pack w2
  return $ case (v1,v2) of
    (Just v1,Just v2) -> 
      (max 0 . min 1 $ V.cosine v1 v2, 
       V.norm2 v1, V.norm2 v2, V.dimShared v1 v2)
    _ -> (-1,-1,-1,-1)

main = do
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: bow-pairs-sim2 <bow matrix> <list of word pairs>"
    exitFailure
  m  <- Bow.readMatrix (args!!0)
  ps <- map (parse . reverse . words) . lines <$> readFile (args!!1)
  putStrLn "w_1\tw_2\tcosine\tnorm(w_1)\tnorm(w_2)\tdim_shared"
  Bow.runModelIO m $ do
--    Bow.setCacheSize 100
    forM_ ps $ \(w1,w2) -> do
      (s,n1,n2,d) <- similarity w1 w2
      liftIO . putStrLn $ printf "%s\t%s\t%.3f\t%.3f\t%.3f\t%d" w1 w2 s n1 n2 d
  where parse (w2:w1:_) = (parseWord w1,parseWord w2)
        parse _         = error "no parse"

