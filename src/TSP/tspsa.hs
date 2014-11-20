import Data.Array.Unboxed
import System.Random
import qualified SA
import qualified Two_opt

type Vertex = Int
type Path = [Vertex]
type Size = Int
type Distance = Double
type Coordinate = Double
type CoArray = UArray Vertex Coordinate

neighbours distf (a:c:rands) (sc, st) = ((score distf state, state), rands)
	where state = makeChange (min a c) (max a c) st
score distf s = sum $ zipWith (curry distf) s (tail s)

-- dist' dist (a, b, c, d) = (dist (a, b) + dist (c, d) - (dist (a, c) + dist (b, d)), a, c)

makeChange a c path = take a path ++ reverse (take (c - a) (drop a path)) ++ drop c path

storeCoords :: Size -> String -> (CoArray, CoArray)
storeCoords n s = (listArray (1, n) $ map fst lst, listArray (1, n) $ map snd lst)
	where	lst = take n . map (lst2 . map read . words) . lines $ s
		lst2 [a, b] = (a, b)

distC :: (CoArray, CoArray) -> (Vertex, Vertex) -> Distance
distC (s, d) (a, b) = euc2 (s ! a) (d ! a) (s ! b) (d ! b)

euc2 x1 y1 x2 y2 = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 


main :: IO ()
main = do
	nLine <- getLine
	let n = read nLine :: Size
	--dLines <- B.getContents
	dLines <- getContents
	let distf = distC (storeCoords n dLines)
	gen <- newStdGen
	gen2 <- newStdGen
	minPath1 <- SA.optimize (randoms gen :: [Double]) (neighbours distf, randomRs (1, n) gen2 :: [Vertex]) (score distf $ [1..n] ++ [1], [1..n] ++ [1])
	let twoO = Two_opt.optimize distf $ snd minPath1
	let minPath = (sum $ zipWith (curry distf) twoO (tail twoO), twoO)
	putStrLn $ show (fst minPath) ++ " 0"
	mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd minPath
        putStrLn ""
