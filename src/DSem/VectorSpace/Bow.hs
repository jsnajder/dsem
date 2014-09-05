{-------------------------------------------------------------------------------

 DSem.VectorSpace.Bow
 Bag-of-words distributional model

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

TODO: split into: 
Bow.Sparse
Bow.Dense
Bow.*.Cached

!!! THIS IS A TEMPORARY VERSION !!!
NB: This version reads in vectors from file, but does not cache.
Implement various variants, also with an interface to a database!

-------------------------------------------------------------------------------}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module DSem.VectorSpace.Bow (
  module DSem.VectorSpace,
  Target,
  Context,
  Bow,
  ModelM,
  Vect,
  readModel,
  readMatrix) where

import DSem.VectorSpace
import qualified DSem.Vector.SparseVector as V
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.IntMap as IM
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
type TargetIndex = DB.Index   -- from targets to fileseeks

data Bow = Bow {
  handle    :: !Handle,
  index     :: !TargetIndex,
  contexts  :: Maybe Contexts }
  deriving (Show,Eq)

instance Model ModelM Target Context Vect where
 
  getVector = readVector

  getDim = do t <- gets (M.size . index)
              c <- gets (IM.size . fromMaybe IM.empty . contexts)
              return (t,c)

  getContexts = gets (IM.elems . fromMaybe IM.empty . contexts) 

  getTargets = gets (M.keys . index)

readVector :: Target -> ModelM (Maybe Vect)
readVector t = do
  h  <- gets handle
  ti <- gets index
  case M.lookup t ti of
    Nothing -> return Nothing
    Just i  -> do liftIO $ hSeek h AbsoluteSeek (toInteger i)
                  Just . parseVector <$> liftIO (T.hGetLine h)

readContexts :: FilePath -> IO Contexts
readContexts f = 
  IM.fromList . zip [1..] . T.lines <$> T.readFile f

readMatrix :: FilePath -> IO Bow
readMatrix f = do
  h  <- openFile f ReadMode
  ti <- DB.mkIndex h
  return $ Bow { handle = h, contexts = Nothing, index = ti }

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

