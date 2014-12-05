module MainNN where

import TSP
import TSP.NN

import Data.Array.Unboxed

mainNN :: a -> b -> IO ()
mainNN _ _ = do
	nLine <- getLine
	let n = read nLine :: Size
	fLines <- getContents
	let dist = distC (storeCoords n fLines)
--	let dist = (!) (getDistances n fLines)
	let path = optimize dist n
--	let paths = map (findPath n dist) [1..n]
--	let path = minimum $ map (pathLen dist &&& id) paths
        putStrLn $ show (fst path) ++ " 0"
--	mapM_ (putStr . (++ " ") . show) . take n $ snd path
	mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd path
        putStrLn ""
