{-------------------------------------------------------------------------------

 A mapper/reducer for Hadoop streams for building BoW models from
 a CONLL-parsed file.

 (c) 2013 Jan Snajder

-------------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}

import ConllReader
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Function (on)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Counts as C
import qualified IdMap as IM
import System.Console.ParseArgs
import System.Environment
import System.IO

type Word           = Text
type Index          = Int
type WordCounts     = C.Counts Text
type ContextIndex   = IM.IdMap Index Word

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

-- extracts +-n sublists
windows :: Int -> [a] -> [(a,[a])]
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

mrReduce :: [(Word,Index)] -> [(Word,[(Index,Int)])]
mrReduce xs = vs
  where cs = C.counts $ C.fromList xs
        ys = map (\((t,c),f) -> (t,(c,f))) cs
        vs = [ (t,map snd x) | x@((t,_):_) <- groupBy ((==) `on` fst) ys]

mkModel
  :: Set Word
  -> ContextIndex 
  -> Int 
  -> (Token -> [String]) 
  -> (Token -> [String]) 
  -> Corpus
  -> [(Word,[(Index,Int)])]
mkModel ts ci n ft fc = mrReduce . concatMap (mrMap ts ci n ft fc)

showVec :: (Word, [(Index,Int)]) -> Text
showVec (t,cf) = T.intercalate "\t" $ t : map (\(c,f) -> 
                   T.pack (show c ++ ":" ++ show f)) cf

readFile' :: FilePath -> IO Text
readFile' f = do
  h <- openFile f ReadMode
  hSetEncoding h utf8
  T.hGetContents h

arg = 
  [ Arg 0 (Just 't') (Just "targets") 
      (argDataRequired "filename" ArgtypeString) "targets list"
  , Arg 1 (Just 'c') 
      (Just "contexts") (argDataRequired "filename" ArgtypeString) 
      "contexts list"
  , Arg 2 (Just 'w') (Just "window") 
      (argDataDefaulted "size" ArgtypeInt 5) 
      "window size around target (default=5)"
  , Arg 3 (Just 'f') (Just "fallback") Nothing 
      ("fallback to word-forms if lemma is " ++ unk)
  , Arg 4 Nothing (Just "tlower") Nothing 
      "lowercase targets"
  , Arg 5 Nothing (Just "clower") Nothing 
      "lowercase contexts"
  , Arg 6 Nothing (Just "tpos") Nothing 
      "targets have coarse POS tag attached"
  , Arg 7 Nothing (Just "cpos") Nothing 
      "contexts have coarse POS tag attached"
  , Arg 8 Nothing Nothing  (argDataRequired "filename" ArgtypeString)
      "corpus in CoNLL format" ]

format :: Bool -> Bool -> Bool -> (Token -> [String])
format False False False = lemma
format False False True  = lemma'
format False True  False = lemmaPos
format False True  True  = lemmaPos'
format True  False False = map lower . lemma
format True  False True  = map lower . lemma'
format True  True  False = map (\(l,p) -> lower l ++ posSep ++ p) . getLP
format True  True  True  = map (\(l,p) -> lower l ++ posSep ++ p) . getLP'

lower = map toLower

main = do
  args <- parseArgsIO ArgsComplete arg
  let tf = getRequiredArg args 0
      cf = getRequiredArg args 1
      w  = getRequiredArg args 2
      ft = format (gotArg args 4) (gotArg args 6) (gotArg args 3)
      fc = format (gotArg args 5) (gotArg args 7) (gotArg args 3)
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  ts <- S.fromList . map (head . T.words) . T.lines <$> readFile' tf
  ci <- (IM.fromList1 . map (head . T.words) . T.lines) <$> readFile' cf
  c  <- readCorpus $ getRequiredArg args 8
  T.putStr . T.unlines . map showVec $ mkModel ts ci w ft fc c

