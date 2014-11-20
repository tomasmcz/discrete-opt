module Main where

import ACO
import Data.Array.Unboxed
--import qualified Data.ByteString.Char8 as B
--import Data.ByteString.Lex.Double

type Vertex = Int
type Size = Int
type Distance = Double
type Coordinate = Double
type CoArray = UArray Vertex Coordinate

storeCoords :: Size -> String -> (CoArray, CoArray)
storeCoords n s = (listArray (1, n) $ map fst lst, listArray (1, n) $ map snd lst)
	where	lst = take n . map (lst2 . map read . words) . lines $ s
		lst2 [a, b] = (a, b)
--	where	readList = unfoldr (readDouble . B.dropWhile isSpace) :: B.ByteString -> [Distance]

distC :: (CoArray, CoArray) -> (Vertex, Vertex) -> Distance
distC (s, d) (a, b) = euc2 (s ! a) (d ! a) (s ! b) (d ! b)

euc2 :: Coordinate -> Coordinate -> Coordinate -> Coordinate -> Distance
euc2 x1 y1 x2 y2 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

readMatrix n = listArray ((1,1), (n,n)) . map read . words 

distF2dist n distf = listArray ((1,1), (n, n)) . map distf $ [(a, b) | a <- [1..n], b <- [1..n]]
-- TODO: zkontrolovat, jestli pole není transponované

main :: IO ()
main = do
	nLine <- getLine
	let n = read nLine :: Size
	--dLines <- B.getContents
	dLines <- getContents
	let dist = distF2dist n $ distC (storeCoords n dLines)
	--let dist = readMatrix n dLines
	minPath <- optimize n dist
	putStrLn $ show (fst minPath) ++ " 0"
	mapM_ (putStr . (++ " ") . show . (+ (-1))) . take n $ snd minPath
        putStrLn ""
