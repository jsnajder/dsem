{-------------------------------------------------------------------------------

 Builds a WxLW model from a distributional memory by rearranging the tensor
 elements into a WxLW matrix. Targets are retained and output as strings,
 whereas contexts are replaced by indices.

 Hadoop version. Use "-m" for mapper and "-r" for reducer.

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

-------------------------------------------------------------------------------}

import Data.List
import Data.List.Split
import Control.Monad
import System.Environment
import qualified Data.Set as S
import qualified Data.ByteString.UTF8 as B
import qualified IdMap as IM
import Data.Word (Word32)
import System.IO
import Data.Maybe

type Word   = B.ByteString
type Link   = B.ByteString
type Weight = Double
type Index  = Word32
type WMap   = IM.IdMap Index Word
type LMap   = IM.IdMap Index Link

mrMap :: (S.Set String) -> LMap -> WMap -> String -> String
mrMap ts lm wm = unlines . mapMaybe (parse . words) . lines
  where parse (w1:l:w2:w:_) 
          | w1 `S.member` ts =
              let l'  = IM.lookup' lm (B.fromString l)
                  w2' = IM.lookup' wm (B.fromString w2)
              in case (l',w2') of
                (Just l,Just w2) -> Just $ w1 ++ "\t" ++ 
                                    show (ix wm (l,w2)) ++ ":" ++ w
                _ -> Nothing
          | otherwise        = Nothing
        parse _              = error "no parse"

ix :: WMap -> (Index,Index) -> Index
ix wm (l,w2) = l * n + w2 + 1
  where n = fromIntegral $ IM.size wm

mrReduce :: String -> String
mrReduce s = unlines  
  [ showKV (w1,map snd xs) | 
    xs@((w1,_):_) <- groupBy (equating fst) . map parseKV $ lines s]
  where parseKV l = let (w1:cs:_) = splitOn "\t" l in (w1,cs)
        showKV (k,css) = k ++ "\t" ++ intercalate "\t" css
        equating f x y = f x == f y

main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  args <- getArgs
  if length args == 0 then
    putStrLn "Usage: dm2bow-hadoop [-m <targets list> <l-index> <w2-index>] [-r]"
  else case args!!0 of
      "-m" -> do
        ts <- (S.fromList . lines) `liftM` readFile (args!!1)
        lm <- (IM.fromList . map B.fromString . lines) `liftM` 
              readFile (args!!2)
        wm <- (IM.fromList . map B.fromString . lines) `liftM` 
              readFile (args!!3)
        interact (mrMap ts lm wm)
      "-r" -> interact mrReduce
      _    -> error "missing option -m or -r"

