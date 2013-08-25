{-------------------------------------------------------------------------------

 Builds a WxLW model from a distributional memory by rearranging the tensor
 elements into a WxLW matrix. Targets are retained and output as strings,
 whereas contexts are replaced by indices.

 NB: Index is built for selected targets only. Consequently, models built for 
 different targets cannot be combined because the indices won't match.

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

{-# LANGUAGE BangPatterns #-}

import Data.List
import Control.Monad
import System.Environment
import qualified Data.Set as S
import qualified Data.ByteString.UTF8 as B
import qualified IdMap as IM
import Data.Word (Word32)
import System.IO
import qualified Data.Map.Strict as M
import Text.Printf

type Word   = B.ByteString
type Link   = B.ByteString
type Weight = Double
type Index  = Word32
type WMap   = IM.IdMap Index Word
type LMap   = IM.IdMap Index Link

type Triplet = (Word,Link,Word,Weight)

readDM :: FilePath -> IO [Triplet]
readDM f = (map (parse . words) . lines) `liftM` readFile f
  where parse (w1:l:w2:w:_) = 
          (B.fromString w1,B.fromString l,B.fromString w2,read w)
        parse w = error "no parse"

wordMaps :: [Triplet] -> (LMap,WMap)
wordMaps = foldl' (\(!m1,!m2) (_,!l,!w2,_) -> 
  (IM.insert m1 l, IM.insert m2 w2)) 
  (IM.empty1,IM.empty1)

type DepModel  = M.Map Word (S.Set (Index,Index,Weight))

addTriplet :: LMap -> WMap -> Triplet -> DepModel -> DepModel
addTriplet lm wm (!w1,!l,!w2,!w) = 
  M.insertWith (S.union) w1 
  (case (IM.lookup' lm l, IM.lookup' wm w2) of
     (Just i1,Just i2) -> i1 `seq` i2 `seq` S.singleton (i1,i2,w)
     _ -> S.empty)

mkDepModel :: LMap -> WMap -> [Triplet] -> DepModel
mkDepModel lm wm = foldl' (\dm t -> addTriplet lm wm t dm) M.empty 

target :: Triplet -> Word
target (w1,_,_,w) = w1

showDepModel :: DepModel -> String
showDepModel dm =
  unlines [ B.toString t ++ "\t" ++ showVec v | (t,v) <- M.toAscList dm ]
  where showVec v = intercalate "\t" 
          [ intercalate ":" [show l,show w2,show w] | (l,w2,w) <- S.elems v ]

main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Usage: dm2dep <dist. memory> <targets list> [<l-index> <w-index>]"
    else do
      hPutStr stderr "Reading targets... "
      ts <- (S.fromList . map B.fromString . lines) `liftM` readFile (args!!1)
      hPutStrLn stderr $
        printf "%d targets read." (S.size ts)
      (lm,wm) <- 
        if length args == 2 
          then do
            hPutStr stderr "Indexing contexts... "
            tr <- filter ((`S.member` ts) . target) `liftM` readDM (args!!0)
            return $ wordMaps tr
          else do
            lm <- (IM.fromList . map B.fromString . lines) `liftM` readFile (args!!2)
            wm <- (IM.fromList . map B.fromString . lines) `liftM` readFile (args!!3)
            return (lm,wm)
      hPutStrLn stderr $ 
        printf "%d links and %d words read." (IM.size lm) (IM.size wm)
      hPutStr stderr "Rearranging tensor... "
      -- read over again, so that previous read isn't retained in memory
      tr <- filter ((`S.member` ts) . target) `liftM` readDM (args!!0)
      putStr . showDepModel $ mkDepModel lm wm tr
      hPutStrLn stderr "done."

