{-------------------------------------------------------------------------------

 Distributional Memory

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

module DSem.DMemory where

import qualified IdMap
import qualified Data.ByteString.UTF8 as B
import Control.Monad
import Data.Maybe
import DSem.DModel
import qualified DSem.DVector as DV
import Data.Word
import Data.List

type Word32Index a = IdMap.IdMap Word32 a

type DMemory w l = [((w,l,w),Weight)]
type DMemoryStr  = DMemory String String
type DMemoryIx   = DMemory Int Int

readDMemory :: FilePath -> IO DMemoryStr
readDMemory f = (map (parse . words) . lines) `liftM` readFile f
  where parse (l1:r:l2:w:_) = ((l1,r,l2),read w)

readDMemory2 :: (String -> w) -> (String -> l) -> String -> DMemory w l
readDMemory2 f g = map (parse . words) . lines
  where parse (w1:l:w2:w:_) = ((f w1,g l,f w2),read w)
        parse _ = error "no parse"

{-
readDMemory :: FilePath -> IO (DMemoryStr)
readDMemory f = (map (parse . words) . lines) `liftM` readFile f
  where parse (l1:r:l2:w:_) = 
          ((B.fromString l1,B.fromString r,B.fromString l2),read w)
-}

showDMemory :: DMemoryStr -> String
showDMemory = 
  unlines . map (\((w1,l,w2),w) -> unwords [w1,l,w2,show w])

{-
indexDMemory :: Ord w => DMemory w w -> (Word32Index w,DMemoryIx)
indexDMemory dmem = (ix,dmemIx)
  where ix = IdMap.fromList $ concatMap (\((l1,r,l2),_) -> [l1,r,l2]) dmem
        dmemIx = map (\((l1,r,l2),w) -> 
                   ((ix IdMap.! l1,ix IdMap.! r,ix IdMap.! l2),w)) dmem
-}

-- ovo radi space leackage... zašto?
indexDMemory :: (Ord w, Ord l) => DMemory w l -> (Word32Index w, Word32Index l)
indexDMemory = foldl' (\ix ((w1,l,w2),_) -> ins w1 w2 l ix) (IdMap.empty,IdMap.empty)
  where ins w1 w2 l (wIx,lIx) = 
          (IdMap.insert (IdMap.insert wIx w1) w2, IdMap.insert lIx l)

indexDMemory2 :: Ord w => DMemory w w -> Word32Index w
indexDMemory2 = IdMap.fromList . concatMap (\((l1,r,l2),_) -> [l1,r,l2]) 

--readDModel :: FilePath -> WordIndex -> IO DModel
--readDModel = undefined

-- define w/o maps?
mode1 :: [((i,j,k),a)] -> [((i,(j,k)),a)]
mode1 = map (\((i,j,k),x) -> ((i,(j,k)),x))

mode2 :: [((i,j,k),a)] -> [((j,(i,k)),a)]
mode2 = map (\((i,j,k),x) -> ((j,(i,k)),x))

mode3 :: [((i,j,k),a)] -> [((k,(i,j)),a)]
mode3 = map (\((i,j,k),x) -> ((k,(i,j)),x))

transp :: [((i,j),a)] -> [((j,i),a)]
transp = map (\((i,j),x) -> ((j,i),x))

-- word by link–word
modelWxLW :: (Ord l, Ord w) => DMemory w l -> DModel w (l,w)
modelWxLW = fromAssocList . mode1

-- word–word by link
modelWWxL :: (Ord l, Ord w) => DMemory w l -> DModel (w,w) l
modelWWxL = fromAssocList . transp . mode2

-- word–link by word
modelWLxW :: (Ord l, Ord w) => DMemory w l -> DModel (w,l) w
modelWLxW = fromAssocList . transp . mode3

-- link by word–word
modelLxWW :: (Ord l, Ord w) => DMemory w l -> DModel l (w,w)
modelLxWW = fromAssocList . mode2

convDMemory :: (w1 -> w2) -> (l1 -> l2) -> DMemory w1 l1 -> DMemory w2 l2
convDMemory f g = map (\((w1,l,w2),w) -> ((f w1, g l, f w2),w))

{-
> dmem <- readDMemory "../data/dm.de-test.txt"
> let dm = modelWxLW dmem
> dims dm
(4,78792)
> cosineSim dm (B.fromString "Buch-n") (B.fromString "Kauf-n")
Just 5.9452521448747214e-2
-}

-- word by link–word
vectorWxLW :: (Ord l, Ord w) => w -> DMemory w l -> DV.DVector (l,w)
vectorWxLW t dmem = 
  DV.fromAssocList [(c,w) | ((t',c),w) <- mode1 dmem, t==t']

target :: ((i,j),a) -> i
target ((t,_),_) = t

