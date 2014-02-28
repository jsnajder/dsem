{-------------------------------------------------------------------------------

 CONLL-parsed file reader. 
 CONLL format is defined here: http://ilk.uvt.nl/conll/

 (c) 2013 Jan Snajder

-------------------------------------------------------------------------------}

module ConllReader where

import System.IO
import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe
import Data.Either
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
type DepRel   = String


mkSentence :: [Token] -> Sentence
mkSentence ts = M.fromList [(ix t,t) | t <- ts]

parseLine :: String -> Either String Token
parseLine s = parse $ splitOn "\t" s
  where parse (s1:s2:s3:s4:s5:_:s7:s8:_)
          | all isDigit s1 && all isDigit s7 = Right $ Token {
              ix      = read s1,
              form    = s2,
              lemma   = splitOn "|" s3,
              cpostag = s4,
              postag  = s5,
              dephead = read s7, 
              deprel  = s8 }
          | otherwise = Left $ "Index and DepHead must be integers"
        parse _ = Left $ "Cannot parse"

readCorpusStr :: String -> Corpus
readCorpusStr = 
  map (mkSentence . rights . map parseLine) . splitOn [""] . lines

readCorpus :: FilePath -> IO Corpus
readCorpus f = readCorpusStr <$> readFile f

showCorpus :: Corpus -> String
showCorpus = unlines . map (unlines . map showToken . sentenceTokens)
  where showToken t = intercalate "\t" $ map (\f -> f t) 
          [show . ix, form, intercalate "|" . lemma, cpostag,
           postag, const "_", show . dephead, deprel] 

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
lemmaPos' t = if unknownLemma t then [(form t,postag t)] else lemmaPos t

-- fallbacks to wordform in case of unknown lemma
lemma' :: Token -> [String]
lemma' t = if unknownLemma t then [form t] else lemma t

showLP :: LemmaPos -> String
showLP (l,p) = l ++ posSep ++ p

unknownLemma :: Token -> Bool
unknownLemma = (==[unk]) . lemma

