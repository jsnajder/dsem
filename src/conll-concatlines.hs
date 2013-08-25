import System.IO
import Data.List
import Data.List.Split

delim = "#"

main = interact (unlines . map (intercalate delim) . splitOn [""] . lines)

