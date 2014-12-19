module Main.NN where

import TSP
import TSP.NN

mainNN :: a -> FilePath -> IO ()
mainNN _ file = do
        (n, dist) <- readProblemFunction file
--	let dist = (!) (getDistances n fLines)
	let path = optimize dist n
--	let paths = map (findPath n dist) [1..n]
--	let path = minimum $ map (pathLen dist &&& id) paths
        putStrLn $ show (fst path) ++ " 0"
--	mapM_ (putStr . (++ " ") . show) . take n $ snd path
	mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd path
        putStrLn ""
