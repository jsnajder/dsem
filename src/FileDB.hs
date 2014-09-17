
{-# LANGUAGE BangPatterns #-}

module FileDB where

-- TODO: move cache here!

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Applicative
import System.IO
import Control.Monad.Reader

type Field = T.Text
type Key   = Field
type Row   = [Field]
type Index = M.Map Key Int

data Table = Table {
  handle    :: Handle,
  index     :: Index }
  deriving (Show,Eq)

type TableM = ReaderT Table IO

runDB :: Table -> TableM a -> IO a
runDB = flip runReaderT

getKeys :: TableM [Key]
getKeys = M.keys <$> asks index

getRow :: Key -> TableM (Maybe Row)
getRow k = do
  h  <- asks handle
  ix <- asks index
  case M.lookup k ix of
    Nothing -> return Nothing
    Just i  -> do liftIO $ hSeek h AbsoluteSeek (toInteger i)
                  Just . tail . T.words <$> liftIO (T.hGetLine h)

mkIndex :: Handle -> IO Index
mkIndex h = M.fromList <$>
  (untilM (hIsEOF h) $ do
    !x <- fromIntegral <$> hTell h
    !t <- (T.copy . head . T.words) <$> T.hGetLine h
    return (t, x))

untilM :: IO Bool -> IO a -> IO [a]
untilM p a = do
  t <- p
  if t then return [] else do x <- a; xs <- untilM p a; return (x:xs)

openTable :: FilePath -> IO Table
openTable f = do
  h  <- openFile f ReadMode
  ti <- mkIndex h
  return $ Table { handle = h, index = ti }

