{-------------------------------------------------------------------------------

 DSem.VectModel.DepModel
 Dependency (WxLW) model

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}

module DSem.VectModel.DepModel (
  module DSem.VectModel,
  Word,
  DepModel,
  readModel) where

import DSem.VectModel
import Control.Monad
import qualified DSem.Vector.SparseVector as V
import Data.Word hiding (Word)
import qualified Data.ByteString.UTF8 as B
import qualified Data.Map.Strict as M
import Data.Ix
import Data.List.Split

type Index = Word32
type Word  = B.ByteString

newtype DepModel = DepModel (M.Map Word V.SparseVector)
  deriving (Show,Read,Eq,Ord)

instance VectModel DepModel Word V.SparseVector where
  
  vect (DepModel m) t = M.lookup t m
  
  dims (DepModel m) = case M.size m of
                       0 -> (0, 0)
                       n -> (n, V.size . V.sum $ M.elems m) --expensive!

  toList (DepModel m) = M.toList m
  
readModel :: FilePath -> IO DepModel
readModel f = 
  (DepModel . M.fromList . map (parse . words) . lines) `liftM` readFile f
  where parse (t:cs)    = let t'  = B.fromString t
                              cs' = V.fromAssocList $ map (parse2 . splitOn ":") cs
                          in t' `seq` cs' `seq` (t',cs')
                          -- without the above, a lot of (:) constructors are
                          -- allocated in memory. Solution would be to use
                          -- Data.ByteString.Char8 as input, put then UTF8
                          -- encoding fails. Look for another solution.
        parse _         = error "no parse"
        parse2 [l,w2,w] = (ix (read l::Index,read w2::Index),read w::Double)
        parse2 _        = error "no parse"
        ix              = index ((0,0),(maxBound,maxBound))

