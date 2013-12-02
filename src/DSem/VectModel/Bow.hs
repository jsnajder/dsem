{-------------------------------------------------------------------------------

 DSem.VectModel.Bow
 Bag-of-words distributional model

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module DSem.VectModel.Bow where
{- (
  module DSem.VectModel,
  Word,
  SparseBoW,
  readModel) where -}

import DSem.VectModel
import qualified DSem.Vector.SparseVector as V
import Control.Monad
import qualified Data.ByteString.UTF8 as B
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Monad.Reader
import Control.Monad.State

type Word  = B.ByteString
type Vect = V.SparseVector

type BowM = ModelPure Bow

runModel :: Bow -> BowM a -> a
runModel m = runModelPure m

data Bow = Bow {
  matrix   :: M.Map Word Vect,
  contexts :: IM.IntMap Word }
  deriving (Show,Read,Eq,Ord)

--type Bow = ModelPure SparseBow Word Word V.SparseVector

instance Model (Reader Bow) Word Word Vect where
 
  getVector t = asks (M.lookup t . matrix) 

  getDim = do m <- asks matrix
              return $ case M.size m of
                0 -> (0, 0)
                n -> (n, V.size . V.sum $ M.elems m) --expensive!

  getContexts = asks (IM.elems . contexts)

  getTargets = asks (M.keys . matrix)

{-
  toList (SB m _) = M.toList m
  
  --fromList xs = SB (M.fromList xs) Nothing
-}

--readModel2 :: FilePath -> Bow ()
--readModel2 f = do
--  m <- readModel f
--  return 

readModel :: FilePath -> IO Bow
readModel f = do
  (x:xs) <- lines `liftM` readFile f
  let cs = IM.fromList . zip [1..] . map B.fromString $ words x
      m  = M.fromList $ map (parse . words) xs
  return $ Bow m cs
  where parse (t:xs) = let t'  = B.fromString t
                           xs' = V.fromAssocList $ map parse2 xs
                       in t' `seq` xs' `seq` (t',xs')
        parse _      = error "no parse"
        parse2 x = let (c,_:f) = break (==':') x
                       c' = c `seq` read c
                       f' = f `seq` read f
                   in c' `seq` f' `seq` (c',f')

-- TODO: leaks! check!
readModelDense :: FilePath -> IO Bow
readModelDense f = do
  (x:xs) <- lines `liftM` readFile f
  let cs = IM.fromList . zip [1..] . map B.fromString $ words x
      m  = M.fromList $ map (parse . words) xs
  return $ Bow m cs
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

