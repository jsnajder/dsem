{-------------------------------------------------------------------------------
 
 Applies weighting on BoW raw frequencies 
 
 NB: Works only for sparse BoWs

 (c) 2013 Jan Snajder <jan.snajder@fer.hr>

 TODO: Add PMI and LL weighting schemes

-------------------------------------------------------------------------------}

import System.IO
import System.Environment
import System.Console.ParseArgs
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified DSem.Vector.SparseVector as V
import DSem.Weighting

type Word   = String
type Weight = Double
type Model  = ([Word],[(Word,V.SparseVector)])
type Index  = Int

-- TODO: move to SparseBow
readModel :: FilePath -> IO Model
readModel f = do
  (x:xs) <- lines `liftM` readFile f
  return $ (words x, map (parse . words) xs)
  where parse (t:xs) = (t, V.fromAssocList $ map parse2 xs)
        parse _      = error "no parse"
        parse2 x = let (c,_:f) = break (==':') x
                   in (read c,read f)

showModel :: Model -> String
showModel (cs,vs) = unlines . map (intercalate "\t") $
 cs : map (\(t,v) -> t : showVect v) vs
 where showVect = map (\(i,w) -> show i ++ ":" ++ show w) . V.toAssocList

type Marginals = (M.Map Word Double, V.SparseVector)

-- not good :-( ..space leaks... don't know why
marginals :: Model -> Marginals
marginals (_,vs) = foldl' (\(tm,cm) (t,v) -> 
  let vx  = v `seq` V.nonzeros v
      s   = vx `seq` sum vx
      tm2 = s `seq` tm `seq` M.insert t s tm
      cm2 = v `seq` cm `seq` V.add cm v
  in (tm2,cm2)) (M.empty,V.empty) vs
  --(M.insert t (sum $ V.nonzeros v) tm, V.add cm v)) (M.empty,V.empty) vs

targetMarginals :: Model -> M.Map Word Double
targetMarginals (_,vs) = 
  M.fromList $ map (\(t,v) -> (t,sum $ V.nonzeros v)) vs

contextMarginals :: Model -> V.SparseVector
contextMarginals (_,vs) = V.sum $ map snd vs

lmiWeighting :: Marginals -> Model -> Model
lmiWeighting (tm,cm) (cs,vs) = (cs,map f vs)
  where n = sum $ V.nonzeros cm
        f (t,v) = (t,V.zipWith (\fx fxy ->
          lmi n fx (M.findWithDefault 0 t tm) fxy) cm v)

arg = [
  Arg 0 (Just 'w') Nothing 
    (argDataDefaulted "LMI|PMI|LL" ArgtypeString "LMI")
    "weighting scheme (default=LMI) [other schemes not yet implemented!]",
  Arg 1 Nothing Nothing  (argDataRequired "filename" ArgtypeString)
    "BoW file"]

main = do
  args <- parseArgsIO ArgsComplete arg
  let f = fromJust $ getArg args 1
  m <- readModel f
  let tm = targetMarginals m
  m <- readModel f
  let cm = contextMarginals m
  m <- readModel f
  putStr . showModel $ lmiWeighting (tm,cm) m
