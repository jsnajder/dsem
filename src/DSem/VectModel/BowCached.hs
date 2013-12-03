{-------------------------------------------------------------------------------

 DSem.VectModel.BowCached
 Bag-of-words distributional model

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

TODO: split into: 
Bow.Sparse
Bow.Dense
Bow.*.Cached

-------------------------------------------------------------------------------}

{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module DSem.VectModel.BowCached (
  module DSem.VectModel,
  Target,
  Context,
  BowCached,
  ModelM,
  Vect,
  CacheStats (..),
  readModel,
  setCacheSize,
  getCacheStats) where
  --cacheTargets,
  --cacheAllTargets

import DSem.VectModel
import qualified DSem.Vector.SparseVector as V
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import Data.SmallString 
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.BoundedMap as BM
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import System.IO
import Debug.Trace
import qualified Data.Text as T

type Word    = T.Text -- TODO; convert to smallstring!
type Target  = Word
type Context = Word
type Vect    = V.SparseVector
type ModelM  = ModelIO BowCached

type Contexts    = IM.IntMap Context
type Matrix      = BM.BoundedMap Target Vect
type TargetIndex = M.Map Target Int   -- from targets to fileseeks

data CacheStats = CacheStats {
  hits      :: !Int,
  misses    :: !Int,
  unknowns  :: !Int }
  deriving (Eq,Show,Read,Ord)

data BowCached = BowCached {
  stats     :: CacheStats,
  handle    :: Handle,
  index     :: TargetIndex,
  matrix    :: Matrix,
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

instance Model (ModelIO BowCached) Target Context Vect where
 
  getVector t = do
    m  <- gets matrix
    case BM.lookup t m of
      Nothing -> do v <- readVector t
                    if (isJust v) then do addVector t (fromJust v); incMisses
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

untilM :: IO Bool -> IO a -> IO [a]
untilM p a = do
  t <- p
  if t then return [] else do x <- a; xs <- untilM p a; return (x:xs)

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

mkTargetIndex :: Handle -> IO TargetIndex
mkTargetIndex h = M.fromList <$>
  (untilM (hIsEOF h) $ do
    x <- hTell h
    (t:_) <- T.words <$> T.hGetLine h
    return (t, fromInteger x))

readContexts :: FilePath -> IO Contexts
readContexts f = 
  IM.fromList . zip [1..] . T.lines <$> T.readFile f

readModel :: FilePath -> FilePath -> IO BowCached
readModel fc fm = do
  h  <- openFile fm ReadMode
  ti <- mkTargetIndex h
  cs <- readContexts fc
  return $ BowCached { 
    matrix = BM.empty defaultCacheSize, 
    handle = h, contexts = Just cs, index = ti,
    stats = CacheStats { hits = 0, misses = 0, unknowns = 0 } }

parseVector :: T.Text -> Vect
parseVector = parse . T.words
  where parse (_:xs) = V.fromAssocList $ map parse2 xs
        parse _      = error "no parse"
        parse2 x = let (c:f:_) = T.split (==':') x
                       c' = read $ T.unpack c
                       f' = read $ T.unpack f
                   in (c',f')

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

