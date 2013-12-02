{-------------------------------------------------------------------------------

 DSem.VectModel.Bow
 Bag-of-words distributional model

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module DSem.VectModel.BowCached where
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
import qualified Data.Map.Strict as MS
import qualified Data.IntMap as IM
import qualified Data.CacheMap as CM
import Data.Maybe
import Control.Monad.State
import Control.Applicative
import System.IO
import Debug.Trace

type Word    = B.ByteString
type Target  = Word
type Context = Word
type Vect    = V.SparseVector
type BowM    = ModelIO BowCached

type Contexts    = IM.IntMap Context
type Matrix      = CM.CacheMap Target Vect
type TargetIndex = M.Map Target Int   -- from targets to fileseeks

data BowCached = BowCached {
 -- cacheSize :: Int,
  handle    :: Handle,
  index     :: TargetIndex,
  matrix    :: Matrix,
  contexts  :: Maybe Contexts }
  deriving (Show,Eq)

runModel :: BowCached -> BowM a -> IO a
runModel = runModelIO

defaultCacheSize = 512

instance Model (ModelIO BowCached) Target Context Vect where
 
  getVector t = do
    m  <- gets matrix
    case CM.lookup t m of
      Nothing -> do v <- readVector t
                    when (isJust v) $ addVector t (fromJust v)
                    return v
      v       -> return v
{-
    case MS.lookup t m of
      Nothing -> do v  <- readVector t
                    cs <- gets cacheSize
                    let m' = MS.insert t v
                            (if MS.size m > cs then MS.deleteMin m else m)
                    modify (\s -> s { matrix = m' })
                    return $ Just v
      v       -> return v
-}

  getDim = do c <- gets (IM.size . fromMaybe IM.empty . contexts)
              t <- gets (M.size . index)
              return (t,c)

  getContexts = gets (IM.elems . fromMaybe IM.empty . contexts) 

  getTargets = gets (M.keys . index)

setCache :: Int -> BowM ()
setCache n = modify (\s -> s { matrix = CM.setCache n $ matrix s})

addVector :: Target -> Vect -> BowM ()
addVector t v = modify (\s -> s { matrix = CM.insert t v $ matrix s} )

untilM :: IO Bool -> IO a -> IO [a]
untilM p a = do
  t <- p
  if t then return [] else do x <- a; xs <- untilM p a; return (x:xs)

readVector :: Target -> BowM (Maybe Vect)
readVector t = do
  h  <- gets handle
  ti <- gets index
  case M.lookup t ti of
    Nothing -> return Nothing
    Just i  -> do liftIO $ hSeek h AbsoluteSeek (toInteger i)
                  Just . parseVector <$> (trace $ "Reading in vector for " ++ B.toString t) 
                                         liftIO (hGetLine h)

mkTargetIndex :: Handle -> IO TargetIndex
mkTargetIndex h = do
  xs <- untilM (hIsEOF h) $ do
    x <- hTell h
    (t:_) <- words <$> hGetLine h
    let t' = B.fromString t
    return $ t' `seq` (t',fromInteger x)
  return $ M.fromList xs

readContexts :: FilePath -> IO Contexts
readContexts f = 
  IM.fromList . zip [1..] . map B.fromString . lines <$> readFile f

readModel :: FilePath -> IO BowCached
readModel f = do
  h  <- openFile f ReadMode
  ti <- mkTargetIndex h
  return $ BowCached { 
    handle = h, matrix = CM.setCache defaultCacheSize $ CM.empty, 
    contexts = Nothing, index = ti }

parseVector :: String -> Vect
parseVector = parse . words
  where parse (_:xs) = V.fromAssocList $ map parse2 xs
        parse _      = error "no parse"
        parse2 x = let (c,_:f) = break (==':') x
                       c' = c `seq` read c
                       f' = f `seq` read f
                   in c' `seq` f' `seq` (c',f')

{-
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

{-
readModel :: FilePath -> IO BowCached
readModel f = do
  (x:xs) <- lines `liftM` readFile f
  let cs = IM.fromList . zip [1..] . map B.fromString $ words x
      m  = M.fromList $ map (parse . words) xs
  return $ undefined -- BowCached 0 undefined m (Just cs)
  where parse (t:xs) = let t'  = B.fromString t
                           xs' = V.fromAssocList $ map parse2 xs
                       in t' `seq` xs' `seq` (t',xs')
        parse _      = error "no parse"
        parse2 x = let (c,_:f) = break (==':') x
                       c' = c `seq` read c
                       f' = f `seq` read f
                   in c' `seq` f' `seq` (c',f')
-}
