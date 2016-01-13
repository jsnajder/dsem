{-------------------------------------------------------------------------------

 A mapper/reducer for Hadoop streams for building BoW models from
 a CONLL-parsed file.

 (c) 2013 Jan Snajder

-------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function (on)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Counts (Counts)
import qualified Data.Counts as C
import qualified IdMap as IM
import Prelude hiding (Word)
import System.Console.ParseArgs
import System.Environment
import System.IO
import System.Random (randomRIO)

type Word           = Text
type Sentence       = [Word]
type Corpus         = [Sentence]
type Index          = Int
type WordCounts     = C.Counts Text
type ContextIndex   = IM.IdMap Index Word

{-
mrMap 
  :: Set Word
  -> ContextIndex 
  -> Int 
  -> (Token -> [String]) 
  -> (Token -> [String]) 
  -> Sentence 
  -> [(Word,Index)]
mrMap ts ci n ft fc = 
  filter ((`S.member` ts) . fst) . concatMap pairs . windows n . sentenceTokens
  where pairs (x,xs) = [(T.pack $ t, c2) | t <- ft x, c <- xs, 
                        c1 <- fc c, Just c2 <- [IM.lookup' ci $ T.pack c1]]
-}

-- Targets found in a sentence
sentenceTargets :: (Word -> Word) -> Set Word -> Sentence -> Set Word
sentenceTargets ft ts = 
   S.fromList . filter (`S.member` ts) . map ft

sampleSentenceTargets 
  :: SamplingProportions 
  -> (Word -> Word) 
  -> Set Word 
  -> Sentence 
  -> IO (Set Word)
sampleSentenceTargets sp ft ts = sampleTargets sp . sentenceTargets ft ts

{-
mrMap2
  :: Set Word
  -> SamplingProportions
  -> ContextIndex 
  -> Int 
  -> (Token -> [String]) 
  -> (Token -> [String]) 
  -> Sentence 
  -> IO (Counts (Word, Index))
mrMap2 ts sp ci n ft fc s = do
  ts' <- sampleSentenceTargets sp ft ts s
  return . C.fromList . filter ((`S.member` ts') . fst) . 
    concatMap pairs . windows n $ sentenceTokens s
  where pairs (x,xs) = [(T.pack $ t, c2) | t <- ft x, c <- xs, 
                        c1 <- fc c, Just c2 <- [IM.lookup' ci $ T.pack c1]]
-}

mrMap3
  :: Set Word
  -> SamplingProportions
  -> ContextIndex 
  -> Int 
  -> (Word -> Word) 
  -> (Word -> Word) 
  -> Sentence
  -> IO (Counts (Word, Index))
mrMap3 ts sp ci n ft fc s = do
  ts' <- sampleSentenceTargets sp ft ts s
  return . C.fromList . filter ((`S.member` ts') . fst) . 
    concatMap pairs $ windows n s
  where pairs (x, xs) = [(ft x, c2) | c <- xs, 
                         Just c2 <- [IM.lookup' ci $ fc c]]

-- extracts +-n sublists
windows :: Int -> [a] -> [(a, [a])]
windows k ws = 
  [ (w, takeFromTo l1 l2 ws ++ takeFromTo r1 r2 ws) |  
    (w,i) <- xs, let (l1,l2) = (i-k,i-1), let (r1,r2) = (i+1,i+k) ]
  where xs = zip ws [0..]

takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo i j
  | i < 0 && j < 0 = const []
  | i < 0          = takeFromTo 0 j
  | j < 0          = takeFromTo i 0
  | otherwise      = take (j-i+1) . drop i

mrReduce :: [(Word, Index)] -> [(Word, [(Index, Int)])]
mrReduce xs = vs
  where cs = C.counts $ C.fromList xs
        ys = map (\((t,c),f) -> (t,(c,f))) cs
        vs = [ (t,map snd x) | x@((t,_):_) <- groupBy ((==) `on` fst) ys]

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

mkModel
  :: Set Word
  -> SamplingProportions
  -> ContextIndex
  -> Int
  -> (Word -> Word)
  -> (Word -> Word)
  -> Corpus
  -> IO [(Word, [(Index, Int)])]
mkModel ts sp ci n ft fc c = do
  xs <- foldM' (\cs s -> do
    cs' <- mrMap3 ts sp ci n ft fc s
    return $ cs `seq` cs `C.union` cs') C.empty c
  let cs = C.counts xs
      ys = map (\((t, c),f) -> (t, (c, f))) cs
      vs = [ (t, map snd x) | x@((t, _):_) <- groupBy ((==) `on` fst) ys]
  return vs

showVec :: (Word, [(Index,Int)]) -> Text
showVec (t,cf) = T.intercalate "\t" $ t : map (\(c,f) -> 
                   T.pack (show c ++ ":" ++ show f)) cf

readFile' :: FilePath -> IO Text
readFile' f = do
  h <- openFile f ReadMode
  hSetEncoding h utf8
  T.hGetContents h

type SamplingProportions = Map Text Double

readProportions :: FilePath -> IO SamplingProportions
readProportions f = 
  M.fromList . map (parse . words) . lines <$> readFile f
  where parse (w:p:_) = (T.pack w, read p)

flipCoin :: Double -> IO Bool
flipCoin p = (<=p) <$> randomRIO (0, 1)

sampleTargets :: SamplingProportions -> Set Word -> IO (Set Word)
sampleTargets sp ws = S.fromList <$> (filterM flipCoin' $ S.toList ws)
  where flipCoin' w = case M.lookup w sp of
                        Just p  -> flipCoin p
                        Nothing -> return True

arg = 
  [ Arg 0 (Just 't') (Just "targets") 
      (argDataRequired "filename" ArgtypeString) "targets list"
  , Arg 1 (Just 'c') 
      (Just "contexts") (argDataRequired "filename" ArgtypeString) 
      "contexts list"
  , Arg 2 (Just 'w') (Just "window") 
      (argDataDefaulted "size" ArgtypeInt 5) 
      "window size around target (default=5)"
  , Arg 3 Nothing (Just "tlower") Nothing 
      "lowercase corpus targets"
  , Arg 4 Nothing (Just "clower") Nothing 
      "lowercase corpus contexts"
  , Arg 5 (Just 'p') (Just "proportions")  
    (argDataOptional "filename" ArgtypeString)
    "target words sampling proportions, one word per line"
  , Arg 6 Nothing Nothing  (argDataRequired "corpus" ArgtypeString)
      "corpus (tokenized, one line per sentence)" ]

main = do
  args <- parseArgsIO ArgsComplete arg
  let tf = getRequiredArg args 0
      cf = getRequiredArg args 1
      w  = getRequiredArg args 2
      ft = if gotArg args 3 then T.toLower else id
      fc = if gotArg args 4 then T.toLower else id
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  ts <- S.fromList . map (head . T.words) . T.lines <$> readFile' tf
  ci <- (IM.fromList1 . map (head . T.words) . T.lines) <$> readFile' cf
  sp <- case getArg args 5 of
          Nothing -> return M.empty
          Just f  -> readProportions f
  c  <- map T.words . T.lines <$> (readFile' $ getRequiredArg args 6)
  (T.unlines . map showVec <$> mkModel ts sp ci w ft fc c) >>= T.putStr

