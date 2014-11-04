{-------------------------------------------------------------------------------

 DSem.Vector.SparseVector
 Sparse distributional vector representation

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveFoldable, InstanceSigs #-} --TMP

module DSem.Vector.SparseVector
  ( module DSem.Vector
  , readVector
  , SparseVector ) where

import Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import DSem.Vector
import Data.Word (Word64)

type Index = Word64
newtype SparseVector = SV (M.Map Index Weight)
  deriving (Eq, Show, Read, Ord)

instance Vector SparseVector where

  empty = SV M.empty

  size (SV v) = fromIntegral $ M.size v

  zipWith f (SV v1) (SV v2) =
    SV $ M.filter (/=0) $ 
    zipAligned (\w1 w2 -> f (fromMaybe 0 w1) (fromMaybe 0 w2)) v1 v2
  
  --map f (SV v) = SV (M.map f v)
  map f (SV v) = SV (M.map (\x -> x `seq` f x) v) -- no effect
        
--  add (SV v1) (SV v2) = SV . M.filter (/=0) $ M.unionWith (+) v1 v2
  add (SV v1) (SV v2) = SV . M.filter (/=0) $ M.unionWith (\x1 x2 -> x1 `seq` x2 `seq` x1+x2 ) v1 v2  -- the same, though

  --pmul (SV v1) (SV v2) = SV $ M.intersectionWith (*) v1 v2
  pmul (SV v1) (SV v2) = SV $ M.intersectionWith (\x1 x2 -> x1 `seq` x2 `seq` x1*x2) v1 v2  -- all the same, though

  --dot (SV v1) (SV v2) = L.sum . M.elems $ M.intersectionWith (*) v1 v2
  dot (SV v1) (SV v2) = foldl' (+) 0 . M.elems $ M.intersectionWith (\x1 x2 -> x1 `seq` x2 `seq` x1*x2) v1 v2  -- a bit better (481 -> 463 MB)
 
  nonzeroWeights (SV v) = M.elems v

  nonzeroes = size

  toList (SV v) = extendVector $ M.toAscList v

  fromList = SV . M.fromList . filter ((/= 0) . snd) . zip [1..]

  toAssocList (SV v) = M.toList v
  
  fromAssocList = SV . M.fromList . filter ((/= 0) . snd)

-- vector dimensions are 1-based !
extendVector :: [(Index,Weight)] -> [Weight]
extendVector = pad 1
  where pad _ []  = []
        pad j zs@((i,w):xs) | j < i     = 0 : pad (j+1) zs
                            | otherwise = w : pad (i+1) xs

-- zips two maps by applying combination function on keys that match
-- for the rest of entries, combination function is applied only on one of the
-- two maps
zipAligned :: Ord k => 
  (Maybe a -> Maybe b -> c) -> M.Map k a -> M.Map k b -> M.Map k c
zipAligned f m1 m2 = M.unions $
  [M.intersectionWith (\x y -> f (Just x) (Just y))  m1 m2, 
   M.map (\x -> f (Just x) Nothing) (M.difference m1 m2), 
   M.map (\x -> f Nothing (Just x)) (M.difference m2 m1)]
-- traversing a sorted list would probably be faster

-- Reads in vector from text.
-- Vector can either be in sparse format:
--   target index_1:weight_1 index:weight_2 ...
-- or dense:
--   target weight_1 weight 2 ...
readVector :: Text -> Maybe SparseVector
readVector s = case T.words s of
  (_:xs) -> Just . fromAssocList $ L.zipWith parse [1..] xs
  _      -> Nothing
  where 
    parse i x = case T.split (==':') x of
                  (i:w:_) -> (read $ T.unpack i, read $ T.unpack w)
                  (w:_)   -> (i, read $ T.unpack w)
                  _       -> error "no parse"
 
