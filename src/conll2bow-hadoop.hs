{-------------------------------------------------------------------------------

 A mapper/reducer for Hadoop streams for building BoW models from
 a CONLL-parsed file.

 NB: mapper takes as input a CONLL-parsed file 
 (one sentence per line, #-separated tokens)
 NB: first line (containing tab-separated contexts elements) must be 
 manually added to the file that is the result of hadoop

 (c) 2013 Jan Snajder

-------------------------------------------------------------------------------}

import System.IO
import System.Environment
import System.Console.ParseArgs
import Data.Either
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad
import qualified Data.ByteString.UTF8 as B
import qualified Data.Counts as C
import qualified IdMap as IM
import qualified Data.Set as S
import ConllReader
import Prelude hiding (Word)

type Word           = String
type Index          = Int
type WordCounts     = C.Counts B.ByteString
type ContextIndex   = IM.IdMap Index Word

delim = "#"

mrMap :: [Word] -> ContextIndex -> Int -> (Token -> [String]) -> 
  (Token -> [String]) -> String -> String
mrMap ts ci n ft fc = 
  unlines . map showPair . filter (isTarget . fst) . concatMap pairs . 
  concatMap (windows n . parse) . lines
  where parse = rights . map parseLine . filter (not . null) . splitOn delim
        pairs (x,xs) = [(t,c2) | t <- ft x, c <- xs, 
                                 c1 <- fc c, Just c2 <- [IM.lookup' ci c1]]
        showPair (t,c) = t ++ "\t" ++ show c
        ts' = S.fromList ts
        isTarget = (`S.member` ts')

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

mrReduce :: IO ()
mrReduce = do
  xs <- getContents
  let cs = C.counts . C.fromList . map (parse . splitOn "\t") $ lines xs
      ys = map (\((t,c),f) -> (t,(c,f))) cs
      vs = [ (t,map snd x) | x@((t,_):_) <- groupBy (equating fst) ys]
  putStr . unlines $ map showVec vs
  where parse (t:c:_)  = (t, read c::Index)
        parse _        = error "no parse"
        showVec (t,cf) = intercalate "\t" $ t : map (\(c,f) -> 
                           show c ++ ":" ++ show f) cf
        equating f x y = f x == f y

expand1 :: Int -> [(Int,Int)] -> [Int]
expand1 n = exp 1
  where exp j [] = if j==n+1 then [] else 0 : exp (j+1) []
        exp j ys@((i,x):xs) = if j==n+1 then [] else
          if i==j then x : exp (j+1) xs else 0 : exp (j+1) ys

readFile' :: FilePath -> IO String
readFile' f = do
  h <- openFile f ReadMode
  hSetEncoding h utf8
  hGetContents h

arg = [
  Arg 0 (Just 'm') (Just "map") Nothing 
    "run as mapper (default)",
  Arg 1 (Just 'r') (Just "reduce") Nothing 
    "run as reducer",
  Arg 2 (Just 't') (Just "targets") 
    (argDataOptional "filename" ArgtypeString) "targets list",
  Arg 3 (Just 'c') 
    (Just "contexts") (argDataOptional "filename" ArgtypeString) 
    "contexts list",
  Arg 4 (Just 'w') (Just "window") 
    (argDataDefaulted "size" ArgtypeInt 5) 
    "window size around target (default=5)",
  Arg 7 (Just 'f') (Just "fallback") Nothing 
    ("fallback to word-forms if lemma is " ++ unk),
  Arg 8 Nothing (Just "tlower") Nothing 
    "lowercase targets",
  Arg 9 Nothing (Just "clower") Nothing 
    "lowercase contexts",
  Arg 10 Nothing (Just "tpos") Nothing 
    "attach coarse POS tag to targets",
  Arg 11 Nothing (Just "cpos") Nothing 
    "attach coarse POS tag to contexts"]

format :: Bool -> Bool -> Bool -> (Token -> [String])
format False False False = lemma
format False False True  = lemma'
format False True  False = map showLP . lemmaPos
format False True  True  = map showLP . lemmaPos'
format True  False False = map lower . lemma
format True  False True  = map lower . lemma'
format True  True  False = map (\(l,p) -> lower l ++ posSep ++ p) . lemmaPos
format True  True  True  = map (\(l,p) -> lower l ++ posSep ++ p) . lemmaPos'

lower = map toLower

main = do
  args <- parseArgsIO ArgsComplete arg
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  if gotArg args 1 then mrReduce else
    case (getArg args 2,getArg args 3) of
         (Just tf,Just cf) -> do
              let w  = getRequiredArg args 4
                  ft = format (gotArg args 8) (gotArg args 10) (gotArg args 7)
                  fc = format (gotArg args 9) (gotArg args 11) (gotArg args 7)
              ts <- lines `liftM` readFile' tf
              ci <- (IM.fromList1 . lines) `liftM` readFile' cf
              interact $ mrMap ts ci w ft fc
         _ -> do putStrLn "Error: missing targets and/or contexts"
                 putStr $ argsUsage args

