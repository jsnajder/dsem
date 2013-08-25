{-------------------------------------------------------------------------------

 CONLL-parsed file reader. 
 CONLL format is defined here: http://ilk.uvt.nl/conll/

 (c) 2013 Jan Snajder

-------------------------------------------------------------------------------}

module ConllReader where

import System.IO
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List.Split
import qualified Data.Map.Strict as M

unk    = "<unknown>"
posSep = "_"

data Token = Token {
  ix      :: Int,
  form    :: String,
  lemma   :: [String],
  cpostag :: String,
  postag  :: String,
  dephead :: Int,
  deprel  :: String }
  deriving (Show,Ord,Eq)

type Sentence = M.Map Int Token
type Corpus   = [Sentence]
type Lemma    = String
type Pos      = String
type LemmaPos = (Lemma,Pos)

mkSentence :: [Token] -> Sentence
mkSentence ts = M.fromList [(ix t,t) | t <- ts]

parseLine :: String -> Maybe Token
parseLine s = parse $ words s
  where parse (s1:s2:s3:s4:s5:_:s7:s8:_)
          | all isDigit s1 && all isDigit s7 = Just $ Token {
              ix      = read s1,
              form    = s2,
              lemma   = splitOn "|" s3,
              cpostag = s4,
              postag  = s5,
              dephead = read s7, 
              deprel  = s8 }
          | otherwise = Nothing
        parse _ = Nothing

readCorpusStr :: String -> Corpus
readCorpusStr = 
  map (mkSentence . mapMaybe parseLine) . splitOn [""] . 
  map removeblank . lines
  where removeblank = unwords . words

readCorpus :: FilePath -> IO Corpus
readCorpus f = readCorpusStr `liftM` readFile f

type DepRel      = String

headToken :: Sentence -> Token -> Maybe Token
headToken s t = case dephead t of
              0 -> Nothing
              i -> M.lookup i s

sentenceTokens :: Sentence -> [Token]
sentenceTokens = M.elems

depTokens :: Sentence -> Token -> [Token]
depTokens s t = filter ((==ix t) . dephead) $ M.elems s

depTokensBy :: DepRel -> Sentence -> Token -> [Token]
depTokensBy r s = filter ((==r) . deprel) . depTokens s

adjTokens :: Sentence -> Token -> [Token]
adjTokens s t = catMaybes [headToken s t] ++ depTokens s t

lemmaPos :: Token -> [LemmaPos]
lemmaPos t = [(l,cpostag t) | l <- lemma t]

-- fallbacks to wordform in case of unknown lemma
lemmaPos' :: Token -> [LemmaPos]
lemmaPos' t = if lemma t == [unk] then [(form t,postag t)] else lemmaPos t

-- fallbacks to wordform in case of unknown lemma
lemma' :: Token -> [String]
lemma' t = if lemma t == [unk] then [form t] else lemma t

showLP :: LemmaPos -> String
showLP (l,p) = l ++ posSep ++ p

