{-------------------------------------------------------------------------------

 Computes BOW similarity scores over a list of word pairs.
 Generates additional files with data about vector
 dimensions and weights for the two word vectors and their
 intersection.

 (c) 2014 Jan Snajder <jan.snajder@fer.hr>

 TODO: add output management via command args
       code cleanup

-------------------------------------------------------------------------------}

{-# LANGUAGE DoAndIfThenElse #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word (Word64)
import qualified Data.Text as T
import qualified DSem.Vector as V
import DSem.VectorSpace.Bow as Bow
import System.Console.ParseArgs
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data WordSim = WordSim 
  { cosine    :: Double
  , norm1     :: Double
  , norm2     :: Double
  , norm12    :: Double
  , dotProd   :: Double
  , dimShared :: Word64
  , entropy1  :: Double
  , entropy2  :: Double
  , jsDiv     :: Double
  , jaccard   :: Double
  , v1        :: [ (Bow.Target, V.Weight) ]
  , v2        :: [ (Bow.Target, V.Weight) ]
  , v12       :: [ (Bow.Target, V.Weight) ] }

nullWordSim = WordSim (-1) 0 0 0 0 0 0 0 0 0 [] [] []

wordSim :: String -> String -> BowM (Maybe WordSim)
wordSim w1 w2 = do
  v1 <- Bow.getTargetVector w1
  v2 <- Bow.getTargetVector w2
  case (v1,v2) of
    (Just v1,Just v2) -> do 
      v1' <- vectorDims v1
      v2' <- vectorDims v2
      let v12 = V.pmul v1 v2
      v12' <- vectorDims v12
      return . Just $ WordSim 
        { cosine    = max 0 . min 1 $ V.cosine v1 v2
        , norm1     = V.norm2 v1
        , norm2     = V.norm2 v2
        , dotProd   = sum $ V.nonzeroWeights v12
        , norm12    = V.norm2 v12
        , entropy1  = V.entropy v1
        , entropy2  = V.entropy v2
        , dimShared = V.dimShared v1 v2
        , jsDiv     = V.jsDivergence v1 v2
        , jaccard   = V.jaccardIndex v1 v2
        , v1        = v1'
        , v2        = v2'
        , v12       = v12' }
    _ -> return Nothing

arg = 
  [ Arg 0 (Just 'p') (Just "vector-profiles") Nothing
    "output vector profiles (instead of plain similarities)"
  , Arg 1 (Just 'h') (Just "header") Nothing
    "output header with field descriptions"
  , Arg 2 (Just 'r') (Just "remove-words") Nothing
    "remove word pairs from output"
  , Arg 3 (Just 'c') (Just "contexts")  
    (argDataOptional "filename" ArgtypeString)
    "BoW contexts (only required for --vector-profiles)"
  , Arg 4 (Just 'd') (Just "dimensions")
    (argDataDefaulted "integer" ArgtypeString "50")
    "number of vector profile dimensions (default=50)"
  , Arg 5 Nothing Nothing  (argDataRequired "bow" ArgtypeString)
    "BoW matrix filename"
  , Arg 6 Nothing Nothing  (argDataRequired "pairs" ArgtypeString)
    "list of word pairs" ]

header = "w1\tw2\tcos(v1,v2)\tnorm2(v1)\tnorm2(v2)\t\tnorm2(v1*v2)\t\
         \dimShared(v1,v2)\tentropy(v1)\tentropy(v2)\tjsDivergence(v1,v2)\t\
         \jaccard(v1,v2)"

main = do
  args <- parseArgsIO ArgsComplete arg
  let matrix   = fromJust $ getArg args 5
      pairs    = fromJust $ getArg args 6
  ps <- let parse (w1:w2:_) = (w1, w2)
            parse _         = error "no parse"
        in map (parse . words) . lines <$> readFile pairs
  if not $ gotArg args 0 then do
    m <- Bow.readMatrix matrix
    when (gotArg args 1) $ putStrLn header
    Bow.runModelIO m $ do
      forM_ ps $ \(w1,w2) -> do
        s <- fromMaybe nullWordSim <$> wordSim w1 w2
        when (not $ gotArg args 2) . liftIO . putStr $ printf "%s\t%s\t" w1 w2
        liftIO . putStrLn $ printf "%f\t%f\t%f\t%f\t%d\t%f\t%f\t%f\t%f"
          (cosine s) (norm1 s) (norm2 s) (dotProd s) (dimShared s) 
          (entropy1 s) (entropy2 s) (jsDiv s) (jaccard s)
  else do
    when (not $ gotArg args 3) $ usageError args "no contexts file provided"
    let contexts = fromJust $ getArg args 3
        dims     = read $ fromJust $ getArg args 4
    m <- Bow.readModel matrix contexts
    Bow.runModelIO m $ do
    forM_ (zip [1..] ps) $ \(i,(w1,w2)) -> do
      s <- fromMaybe nullWordSim <$> wordSim w1 w2
      liftIO . putStrLn $ 
        printf "(%04d) w1 = %s, w2 = %s, cos(v1,v2) = %.4f, \
               \norm2(v1) = %.2f, norm2(v2) = %.2f, norm2(v1*v2) = %.2f, \
               \dot(v1,v2) = %.2f, dimShared(v1,v2) = %d, \
               \entropy(v1) = %.2f, entropy(v2) = %.2f, \
               \jsDivergence(v1,v2) = %.2f, jaccard(v1,v2) = %.2f\n\n"
          (i::Int) w1 w2 (cosine s) (norm1 s) (norm2 s) (norm12 s) 
          (dotProd s) (dimShared s) (entropy1 s) (entropy2 s)
          (jsDiv s) (jaccard s) ++ 
        printVectorDims dims s

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
vectorDims :: V.Vector v => v -> BowM [(Bow.Target,V.Weight)]
vectorDims v = do
  ts <- Bow.getContexts
  let ws = V.toList v
  return $ zip ts ws

