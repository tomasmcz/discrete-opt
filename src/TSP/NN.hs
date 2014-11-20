import Control.Arrow
import Data.Maybe
import Data.List
--import Data.List.Stream
--import Prelude hiding ((++), lines, map, minimum, splitAt, sum, repeat, tail, take, words, zip)
--TODO zrušit explicitní rekurzi a zkusit znovu
import Data.Array.Unboxed
import qualified Data.Set as Set

import Two_opt

type Vertex = Int
type Path = [Vertex]
type Size = Int
type Distance = Double
type Coordinate = Double
type DArray = UArray (Vertex, Vertex) Distance
type CArray = UArray Vertex Coordinate
type FDist = ((Vertex, Vertex) -> Distance)

rList :: String -> [Distance]
rList = map read . words

getDistances :: Size -> String -> DArray
getDistances n source = listArray ((1, 1), (n, n)) $ take (n * n) (rList source)

findPath :: Size -> FDist -> Vertex -> Path
findPath n dist origin = origin : fp dist origin origin (Set.delete origin (Set.fromAscList [1..n]))

fp dist first last unv | not (Set.null unv) = nearest : fp dist first nearest (Set.delete nearest unv)
	where 	nearest = snd (minimum (map dist' (Set.elems unv)))
		dist' x = (dist (last, x), x)
fp dist first last _ = [first]

allPaths :: FDist -> Size -> [Path]
--allPaths dist n = map (findPath n dist) [1..n]
allPaths dist n = map (Two_opt.optimize dist . findPath n dist) [1..n]

pathLen :: FDist -> Path -> Distance
pathLen dist path = sum . map dist $ zip path (tail path)

allPathlens :: FDist -> Int -> [(Distance, Path)]
allPathlens dist n = map (pathLen dist &&& id) $ allPaths dist n

storeCoords :: Size -> String -> (CArray, CArray)
storeCoords n s = (listArray (1, n) $ map fst lst, listArray (1, n) $ map snd lst)
	where	lst = take n . map (lst2 . map read . words) . lines $ s
		lst2 [a, b] = (a, b)

distC :: (CArray, CArray) -> (Vertex, Vertex) -> Distance
distC (s, d) (a, b) = euc2 (s ! a) (d ! a) (s ! b) (d ! b)

euc2 :: Coordinate -> Coordinate -> Coordinate -> Coordinate -> Distance
euc2 x1 y1 x2 y2 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2) 

-- TODO: dát zvlášť modul pro načítání dat

main :: IO ()
main = do
	nLine <- getLine
	let n = read nLine :: Size
	fLines <- getContents
--	let dist = distC (storeCoords n fLines)
	let dist = (!) (getDistances n fLines)
	let path = minimum $ allPathlens dist n
--	let paths = map (findPath n dist) [1..n]
--	let path = minimum $ map (pathLen dist &&& id) paths
        putStrLn $ show (fst path) ++ " 0"
--	mapM_ (putStr . (++ " ") . show) . take n $ snd path
	mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd path
        putStrLn ""
