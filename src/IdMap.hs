-- (c) 2010 Jan Snajder

module IdMap where

import Data.List
import qualified Data.Map as M
import Data.Maybe 
import Data.Word

class (Enum a, Bounded a, Ord a) => Ix a

instance Ix Int
instance Ix Word8
instance Ix Word16
instance Ix Word32
instance Ix Word64

type Word32Index a = IdMap Word32 a

data IdMap i a = IdMap { 
  next  :: !i,
  idMap :: M.Map a i}
  deriving (Eq,Show,Read,Ord)

empty :: Ix i => IdMap i a 
empty = IdMap {idMap=M.empty, next=toEnum 0}

empty1 :: Ix i => IdMap i a 
empty1 = IdMap {idMap=M.empty, next=toEnum 1}

toList :: IdMap i a -> [(a,i)]
toList = M.toList . idMap

fromList :: (Ix i, Ord a) => [a] -> IdMap i a
fromList = foldl' IdMap.insert empty

fromList1 :: (Ix i, Ord a) => [a] -> IdMap i a
fromList1 = foldl' IdMap.insert empty1

size :: IdMap i a -> Int
size = M.size . idMap

insert :: 
  (Ix i, Ord a) => IdMap i a -> a -> IdMap i a
insert ix = snd . insertLookup ix

insertLookup :: 
  (Ix i, Ord a) => IdMap i a -> a -> (i,IdMap i a)
insertLookup ix@(IdMap {idMap=m, next=i}) x 
  | i < maxBound = case M.lookup x m of
      Nothing -> (i,IdMap {idMap = M.insert x i m,next = succ i}) 
      Just i  -> (i,ix)
  | otherwise = error "id out of range"

lookup' :: Ord a => IdMap i a -> a -> Maybe i
lookup' ix x = M.lookup x (idMap ix)

(!) :: Ord a => IdMap i a -> a -> i
ix ! x = (M.!) (idMap ix) x

lookup :: (Ord a, Show a) => IdMap i a -> a -> i
lookup ix x = case M.lookup x (idMap ix) of
  Nothing -> error $ show x ++ " not in index"
  Just i  -> i

lookupList' :: Ord a => IdMap i a -> [a] -> [i]
lookupList' ix = mapMaybe (lookup' ix)

lookupList :: (Ord a, Show a) => IdMap i a -> [a] -> [i]
lookupList ix = map (IdMap.lookup ix)

showIdMap :: (Show i, Show a) => IdMap i a -> String
showIdMap = 
  unlines . map (\(x,i) -> unwords [show x,show i]) . M.toAscList . idMap

readIdMap :: (Ix i, Read i, Read a, Eq a) => String -> IdMap i a
readIdMap s = IdMap { idMap = M.fromAscList xs, next = toEnum 0 }
  where parse [x,i] = (read x, read i)
        parse _     = error "no parse"
        xs = map (parse . words) $ lines s

