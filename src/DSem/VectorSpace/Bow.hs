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

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module DSem.VectorSpace.Bow
  ( BowM 
  , Context 
  , module DSem.VectorSpace 
  , Target 
  , readModel 
  , readMatrix
  , setTargetConversion ) where

import Prelude hiding (Word)
import Control.Applicative
import Control.Monad.State.Strict
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Char
import Data.Maybe
import Debug.Trace
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import DSem.VectorSpace
import DSem.Vector.SparseVector (readVector, SparseVector) 
import qualified FileDB as DB
import System.IO

type Word    = T.Text -- TODO: convert to smallstring!
type Target  = Word
type Context = Word
type BowM    = ModelIO Bow

type Contexts    = IM.IntMap Context
type TargetIndex = DB.Index   -- from targets to fileseeks

data Bow = Bow 
  { targetConversion :: Target -> Target
  , handle           :: !Handle
  , index            :: !TargetIndex
  , contexts         :: Maybe Contexts }

instance Targetable Target Target where
  fromTarget = id
  toTarget   = id

instance Targetable String Target where
  fromTarget = T.unpack
  toTarget   = T.pack

instance Model BowM Target Context SparseVector where
 
  getVector t = do f <- gets targetConversion
                   loadVector (f t)

  getDim = do t <- gets (M.size . index)
              c <- gets (IM.size . fromMaybe IM.empty . contexts)
              return (t,c)

  getContexts = gets (IM.elems . fromMaybe IM.empty . contexts) 

  getTargets = gets (M.keys . index)

loadVector :: Target -> BowM (Maybe SparseVector)
loadVector t = do
  h  <- gets handle
  ti <- gets index
  case M.lookup t ti of
    Nothing -> return Nothing
    Just i  -> do liftIO $ hSeek h AbsoluteSeek (toInteger i)
                  readVector <$> liftIO (T.hGetLine h)

readContexts :: FilePath -> IO Contexts
readContexts f = 
  IM.fromList . zip [1..] . T.lines <$> T.readFile f

-- Reads in a matrix from a file. Skipps the first line if it contains only
-- digits.
readMatrix :: FilePath -> IO Bow
readMatrix f = do
  h  <- openFile f ReadMode
  l <- T.hGetLine h
  when (not (isHeader l)) $ hSeek h AbsoluteSeek 0
  ti <- DB.mkIndex h
  return $ Bow 
    { targetConversion = id
    , handle   = h
    , contexts = Nothing
    , index    = ti }
  where isHeader = all (T.all isDigit) . T.words  

readModel :: FilePath -> FilePath -> IO Bow
readModel fm fc = do
  m <- readMatrix fm  
  cs <- readContexts fc
  return $ m { contexts = Just cs }

--setTargetHook :: (Target -> Target) -> BowM ()
--setTargetHook f = modify (\bow -> bow { targetHook = f })

{-
class Targetable a where
  toTarget   :: a -> Target
  fromTarget :: Target -> a
-}

setTargetConversion :: Targetable a Target => (a -> a) -> BowM ()
setTargetConversion f =
  modify (\bow -> bow { targetConversion = toTarget . f . fromTarget })

