{-------------------------------------------------------------------------------

 Computes BOW similarity scores over a list of word pairs.
 Optionally generates additional files with data about vector
 dimensions and weights for the two word vectors and their
 intersection.

 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import qualified DSem.VectorSpace.BowCached as Bow
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

data WordSim = WordSim {
  cosine :: Double,
  norm1  :: Double,
  norm2  :: Double,
  norm12 :: Double,
  dimShared :: Int,
  v1  :: [(Bow.Target,V.Weight)],
  v2  :: [(Bow.Target,V.Weight)],
  v12 :: [(Bow.Target,V.Weight)] }

nullWordSim = WordSim (-1) 0 0 0 0 [] [] []

{-
similarity ::
  String -> String -> Bow.ModelM (Maybe (Double,Double,Double,Int))
similarity w1 w2 = do
  v1 <- Bow.getVector $ T.pack w1
  v2 <- Bow.getVector $ T.pack w2
  return $ liftA2 f v1 v2
  where f v1 v2 = (max 0 . min 1 $ V.cosine v1 v2, 
                   V.norm v1, V.norm v2, V.dimShared v1 v2)
-}

wordSim :: String -> String -> Bow.ModelM (Maybe WordSim)
wordSim w1 w2 = do
  v1 <- Bow.getVector $ T.pack w1
  v2 <- Bow.getVector $ T.pack w2
  case (v1,v2) of
    (Just v1,Just v2) -> do 
      v1' <- vectorDims v1
      v2' <- vectorDims v2
      let v12 = V.pmul v1 v2
      v12' <- vectorDims v12
      return . Just $ WordSim {
        cosine = max 0 . min 1 $ V.cosine v1 v2, 
        norm1  = V.norm2 v1,
        norm2  = V.norm2 v2,
        norm12 = V.norm1 v12,
        dimShared = V.dimShared v1 v2,
        v1 = v1',
        v2 = v2',
        v12 = v12' }
    _ -> return Nothing

{-
arg = [
  Arg 0 (Just 'l') (Just "linkage") 
    (argDataDefaulted "a|s|c" ArgtypeString "a")
    "linkage type (default=a)",
  Arg 1 Nothing Nothing  (argDataRequired "bow matrix" ArgtypeString)
    "bow matrix file",
  Arg 2 Nothing Nothing  (argDataRequired "word pairs" ArgtypeString)
    "word pairs file"]
-}

main = do
  args <- getArgs
  when (length args < 3) $ do
    putStrLn "Usage: bowSim <bow matrix> <bow contexts> <list of word pairs>"
    exitFailure
  m  <- Bow.readModel (args!!0) (args!!1)
  ps <- let parse (w2:w1:_) = (parseWord w1,parseWord w2)
            parse _         = error "no parse"
        in map (parse . reverse . words) . lines <$> readFile (args!!2)
  f <- openFile "vector-profiles.txt" WriteMode
  putStrLn "word_1\tword_2\tcosine\tnorm_1\tnorm_2\tdim_shared"
  Bow.runModelIO m $ do
    Bow.setCacheSize 100
    forM_ (zip [(1::Int)..] ps) $ \(i,(w1,w2)) -> do
      c <- fromMaybe nullWordSim <$> wordSim w1 w2
      liftIO . putStrLn $ printf "%s\t%s\t%.3f\t%.3f\t%.3f\t%d" w1 w2 
        (cosine c) (norm1 c) (norm2 c) (dimShared c)
      liftIO . hPutStrLn f $ 
        printf "(%04d) w1 = %s, w2 = %s, cos(v1,v2) = %.3f, norm2(v1) = %.2f, norm2(v2) = %.2f, norm1(v1*v2) = %.2f, dim_shared(v1,v2) = %d\n\n"
          i w1 w2 (cosine c) (norm1 c) (norm2 c) (norm12 c) (dimShared c) ++ 
        printVectorDims 50 c
  hClose f

printVectorDims :: Int -> WordSim -> String
printVectorDims k s = unlines $ zipWith3 f xs1 xs2 xs12
  where xs1 = top k (v1 s)
        xs2 = top k (v2 s)
        xs12 = top k (v12 s) ++ repeat (T.pack "",0)
        top k = map r . take k . filter ((/=0).snd) . 
                sortBy (flip $ comparing snd)
        f (t1,w1) (t2,w2) (t12,w12) = 
          printf "%11.2f %-30s\t%11.2f %-30s\t%11.2f %-30s"
          w1 (T.unpack t1) w2 (T.unpack t2) w12 (T.unpack t12)
        r (_,0) = (T.pack "",0)
        r x = x

-- TODO: move this to Bow module
-- within a Bow monad, you should be able to link dimensions to targets
-- provide a getVector variant that does that
-- and/or a context aware vector type
-- maybe change distributional vector to such a type!
-- returns vector contexts sorted in descending order by weights
vectorDims :: V.Vector v => v -> Bow.ModelM [(Bow.Target,V.Weight)]
vectorDims v = do
  ts <- Bow.getContexts
  let ws = V.toList v
  return $ zip ts ws

