-- (c) 2013 Jan Snajder
--
-- Word counts mapper/reducer for Hadoop streams for CONLL parsed file
-- Counts word forms
--   -w  wordforms
--   -l  lemmas
--   -L  lemmas, with fallback to wordforms in case of '<unknown>'
--   -p  concatenates pos tags

import System.IO
import System.Environment
import Data.Word
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.ByteString.UTF8 as B
import ConllReader
import qualified Data.Counts as C
import Control.Monad
import Data.Either

type WordCounts = C.Counts B.ByteString

xMap :: (Token -> [String]) -> String -> String
xMap f = unlines . concatMap f . rights . map parseLine . lines

mrReduce :: IO ()
mrReduce = do
  xs <- getContents
  putStr . showCounts . C.fromList . map B.fromString $ lines xs

showCounts :: WordCounts -> String
showCounts = 
  unlines . map (\(w,c) -> B.toString w ++ "\t" ++ show c) . C.counts

main :: IO ()
main = do
  args <- getArgs
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  if length args == 0 then 
    putStrLn "Usage: conll2counts-hadoop [-m -[w|l|L] [-p] | -r]"
  else case (args!!0) of
      "-m" -> 
        let mrMap = case (args!!1) of
                  "-w" -> if "-p" `elem` args then
                            xMap (\t -> [form t ++ "_" ++ postag t])
                          else xMap (\t -> [form t])
                  "-L" -> if "-p" `elem` args then
                            xMap (map showLP . lemmaPos')
                          else xMap lemma'
                  _    -> if "-p" `elem` args then
                            xMap (map showLP . lemmaPos)  
                          else xMap lemma
        in interact mrMap
      "-r" -> mrReduce
      _    -> error "missing option -m or -r"


