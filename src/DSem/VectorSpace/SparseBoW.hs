{-------------------------------------------------------------------------------

 DSem.VectModel.SparseBoW
 Bag-of-words distributional model

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module DSem.VectModel.SparseBoW (
  module DSem.VectModel,
  Word,
  SparseBoW,
  readModel) where

import DSem.VectModel
import qualified DSem.Vector.SparseVector as V
import Control.Monad
import qualified Data.ByteString.UTF8 as B
import qualified Data.Map as M
import qualified Data.IntMap as IM

type Word  = B.ByteString

data SparseBoW = SB {
  matrix   :: M.Map Word V.SparseVector,
  contexts :: IM.IntMap Word }
  deriving (Show,Read,Eq,Ord)

instance VectModel SparseBoW Word V.SparseVector where
  
  vect (SB m _) t = M.lookup t m
  
  dims (SB m _) = case M.size m of
                  0 -> (0, 0)
                  n -> (n, V.size . V.sum $ M.elems m) --expensive!

  toList (SB m _) = M.toList m
  
  --fromList xs = SB (M.fromList xs) Nothing

readModel :: FilePath -> IO SparseBoW
readModel f = do
  (x:xs) <- lines `liftM` readFile f
  let cs = IM.fromList . zip [1..] . map B.fromString $ words x
      m  = M.fromList $ map (parse . words) xs
  return $ SB m cs
  where parse (t:xs) = let t'  = B.fromString t
                           xs' = V.fromAssocList $ map parse2 xs
                       in t' `seq` xs' `seq` (t',xs')
        parse _      = error "no parse"
        parse2 x = let (c,_:f) = break (==':') x
                       c' = c `seq` read c
                       f' = f `seq` read f
                   in c' `seq` f' `seq` (c',f')

-- TODO: leaks! check!
readModelDense :: FilePath -> IO SparseBoW
readModelDense f = do
  (x:xs) <- lines `liftM` readFile f
  let cs = IM.fromList . zip [1..] . map B.fromString $ words x
      m  = M.fromList $ map (parse . words) xs
  return $ SB m cs
  where parse (t:cs) = let t'  = B.fromString t
                           cs' = V.fromAssocList . filter ((>0).snd) $ 
                                 let zs = map (\c -> let w = read c :: Double in w `seq` w) cs
                                 in zs `seq` zip [1..] zs
                       in t' `seq` cs' `seq` (t',cs')
                       -- without the above, a lot of (:) constructors are
                       -- allocated in memory. Solution would be to use
                       -- Data.ByteString.Char8 as input, put then UTF8
                       -- encoding fails. Look for another solution.
        parse _      = error "no parse"

