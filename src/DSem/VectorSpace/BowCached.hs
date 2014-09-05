{-------------------------------------------------------------------------------

 DSem.VectorSpace.BowCached
 Bag-of-words distributional model

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

TODO: split into: 
Bow.Sparse
Bow.Dense
Bow.*.Cached

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module DSem.VectorSpace.BowCached (
  module DSem.VectorSpace,
  Target,
  Context,
  Bow,
  ModelM,
  Vect,
  CacheStats (..),
  readModel,
  readMatrix,
  setCacheSize,
  getCacheStats) where

import DSem.VectorSpace
import qualified DSem.Vector.SparseVector as V
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.BoundedMap as BM
import Data.Maybe
import Control.Monad.State.Strict
import Control.Applicative
import System.IO
import qualified FileDB as DB

type Word    = T.Text -- TODO: convert to smallstring!
type Target  = Word
type Context = Word
type Vect    = V.SparseVector
type ModelM  = ModelIO Bow

type Contexts    = IM.IntMap Context
type Matrix      = BM.BoundedMap Target Vect
type TargetIndex = DB.Index   -- from targets to fileseeks

data CacheStats = CacheStats {
  hits      :: !Int,
  misses    :: !Int,
  unknowns  :: !Int }
  deriving (Eq,Show,Read,Ord)

data Bow = Bow {
  stats     :: !CacheStats,
  handle    :: !Handle,
  index     :: !TargetIndex,
  matrix    :: !Matrix,
  contexts  :: Maybe Contexts }
  deriving (Show,Eq)

defaultCacheSize = 512

incHits :: ModelM ()
incHits = modify $ \s -> let st = stats s 
  in s { stats = st { hits = succ $ hits st }}

incMisses :: ModelM ()
incMisses = modify $ \s -> let st = stats s 
  in s { stats = st { misses = succ $ misses st }}

incUnknowns :: ModelM ()
incUnknowns = modify $ \s -> let st = stats s 
  in s { stats = st { unknowns = succ $ unknowns st }}

getCacheStats :: ModelM CacheStats
getCacheStats = gets stats

instance Model ModelM Target Context Vect where
 
  getVector t = do
    m <- gets matrix
    case BM.lookup t m of
      Nothing -> do v <- readVector t
                    if isJust v then do addVector t (fromJust v); incMisses
                      else incUnknowns
                    return v
      v       -> do incHits; return v

  getDim = do t <- gets (M.size . index)
              c <- gets (IM.size . fromMaybe IM.empty . contexts)
              return (t,c)

  getContexts = gets (IM.elems . fromMaybe IM.empty . contexts) 

  getTargets = gets (M.keys . index)

setCacheSize :: Int -> ModelM ()
setCacheSize n = modify (\s -> s { matrix = BM.setBound n $ matrix s})

addVector :: Target -> Vect -> ModelM ()
addVector t v = modify (\s -> s { matrix = BM.insert t v $ matrix s} )
--addVector t v = modify (\s -> let m = matrix s in s `seq` m `seq` t `seq` v `seq` s { matrix = BM.insert t v m} )

readVector :: Target -> ModelM (Maybe Vect)
readVector t = do
  h  <- gets handle
  ti <- gets index
  case M.lookup t ti of
    Nothing -> return Nothing
    Just i  -> do liftIO $ hSeek h AbsoluteSeek (toInteger i)
                  Just . parseVector <$> liftIO (T.hGetLine h)

{-
nepotrebno?

-- not good: mapM is not lazy... or something (?)
-- not tail recursive!
--cacheTargets :: [Target] -> ModelM Int
--cacheTargets ts = length . filter isJust <$> mapM getVector ts

cacheTargets :: [Target] -> ModelM Int
cacheTargets = foldM (\n t -> do Just v <- getVector t; return (V.norm v `seq` n `seq` n + 1)) 0
-- the above forces the vector to be computed

cacheAllTargets :: ModelM Int
cacheAllTargets = getTargets >>= cacheTargets
-}

readContexts :: FilePath -> IO Contexts
readContexts f = 
  IM.fromList . zip [1..] . T.lines <$> T.readFile f

readMatrix :: FilePath -> IO Bow
readMatrix f = do
  h  <- openFile f ReadMode
  ti <- DB.mkIndex h
  return $ Bow { 
    matrix = BM.empty defaultCacheSize, 
    handle = h, contexts = Nothing, index = ti,
    stats = CacheStats { hits = 0, misses = 0, unknowns = 0 } }

readModel :: FilePath -> FilePath -> IO Bow
readModel fm fc = do
  m <- readMatrix fm  
  cs <- readContexts fc
  return $ m { contexts = Just cs }

parseVector :: T.Text -> Vect
parseVector = parse . T.words
  where parse (_:xs) = V.fromAssocList $ map parse2 xs
        parse _      = error "no parse"
        parse2 x = case T.split (==':') x of
                     (c:f:_) -> (read $ T.unpack c, read $ T.unpack f)
                     _       -> error "no parse"

{- TODO: adapt and move to Bow uncached
 
readMatrix :: FilePath -> IO Matrix
readMatrix f = 
  M.fromList . map (parse . words) . lines <$> readFile f
  where parse (t:xs) = let t'  = B.fromString t
                           xs' = V.fromAssocList $ map parse2 xs
                       in t' `seq` xs' `seq` (t',xs')
        parse _      = error "no parse"
        parse2 x = let (c,_:f) = break (==':') x
                       c' = c `seq` read c
                       f' = f `seq` read f
                   in c' `seq` f' `seq` (c',f')
-}

