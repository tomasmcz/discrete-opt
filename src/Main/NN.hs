module Main.NN where

import System.Console.GetOpt

import TSP
import TSP.NN
import Main.Opts

mainNN :: [Flag] -> [String] -> IO ()
mainNN _ ("matrix":file:_) = runNN $ MatrixFile file
mainNN _ ("coords":file:_) = runNN $ Euc2DFile file
mainNN _ _ = putStrLn $ usageInfo header options

runNN :: TSPFile -> IO ()
runNN file = do
        (n, dist) <- readProblemFunction file
	let path = optimize dist n
--	let paths = map (findPath n dist) [1..n]
--	let path = minimum $ map (pathLen dist &&& id) paths
        putStrLn $ show (fst path) ++ " 0"
--	mapM_ (putStr . (++ " ") . show) . take n $ snd path
	mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd path
        putStrLn ""
