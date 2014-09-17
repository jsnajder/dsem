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
import Text.Printf

type Word   = String
type Weight = Double
type Matrix = [(Word,V.SparseVector)]
type Index  = Int

-- TODO: move to SparseBow
readMatrix :: FilePath -> IO Matrix
readMatrix f = do
  xs <- lines `liftM` readFile f
  return $ map (parse . words) xs
  where parse (t:xs) = (t, V.fromAssocList $ zipWith parse2 [1..] xs)
        parse _      = error "no parse"
        parse2 i x = case break (==':') x of
                       (i,_:w) -> (read i, read w)
                       (w,[])  -> (i, read w)

showMatrix :: Matrix -> String
showMatrix m = unlines . map (intercalate "\t") $
 map (\(t,v) -> t : showVect v) m
 where showVect = map (\(i,w) -> printf "%d:%.3f" i w) . V.toAssocList

type Marginals = (M.Map Word Double, V.SparseVector)

-- not good :-( ..space leaks... don't know why
marginals :: Matrix -> Marginals
marginals m = foldl' (\(tm,cm) (t,v) -> 
  let vx  = v `seq` V.nonzeroWeights v
      s   = vx `seq` sum vx
      tm2 = s `seq` tm `seq` M.insert t s tm
      cm2 = v `seq` cm `seq` V.add cm v
  in (tm2,cm2)) (M.empty,V.empty) m
  --(M.insert t (sum $ V.nonzeros v) tm, V.add cm v)) (M.empty,V.empty) vs

targetMarginals :: Matrix -> M.Map Word Double
targetMarginals = M.fromList . map (\(t,v) -> (t,sum $ V.nonzeroWeights v)) 

contextMarginals :: Matrix -> V.SparseVector
contextMarginals = V.sum . map snd

lmiWeighting :: Bool -> Marginals -> Matrix -> Matrix
lmiWeighting nz (tm,cm) = map f
  where n = sum $ V.nonzeroWeights cm
        f (t,v) = (t,V.zipWith (\fx fxy ->
          let w = lmi n fx (M.findWithDefault 0 t tm) fxy
          in if nz then max 0 w else w) cm v)

arg = [
  Arg 0 (Just 'w') Nothing 
    (argDataDefaulted "LMI|PMI|LL" ArgtypeString "LMI")
    "weighting scheme (default=LMI) [other schemes not yet implemented!]",
  Arg 1 (Just 'n') (Just "nonnegative") Nothing
    "retain only non-negative weights",
  Arg 2 Nothing Nothing  (argDataRequired "filename" ArgtypeString)
    "BoW file"]

main = do
  args <- parseArgsIO ArgsComplete arg
  let f = fromJust $ getArg args 2
  m <- readMatrix f
  let tm = targetMarginals m
  m <- readMatrix f
  let cm = contextMarginals m
  m <- readMatrix f
  putStr . showMatrix $ lmiWeighting (gotArg args 1) (tm,cm) m

