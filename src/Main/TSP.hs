module Main.TSP where

import Control.Monad.Random
import System.Console.GetOpt
import System.Random ()

import ACO
import SA
import TSP
import TSP.NN
import qualified TSP.TwoOpt
import Main.Opts

saConfig :: SA.Config
saConfig = SA.Config 300000 0.99 100 7 10

mainSA :: [Flag] -> [String] -> IO ()
mainSA _ ("matrix":file:_) = runSA $ MatrixFile file
mainSA _ ("coords":file:_) = runSA $ Euc2DFile file
mainSA _ _ = putStrLn $ usageInfo header options

runSA :: TSPFile -> IO ()
runSA file = do
  (n, distf) <- readProblemFunction file
  (minPath1, _) <- evalRandIO $ SA.optimize
                     saConfig
                     (neighbours distf n)
                     (score distf $ [1..n] ++ [1], [1..n] ++ [1]) -- TODO random initial state
  let twoO = TSP.TwoOpt.optimize distf $ snd minPath1
  let minPath = (sum $ zipWith (curry distf) twoO (tail twoO), twoO)
  putStrLn $ show (fst minPath) ++ " 0"
  mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd minPath
  putStrLn ""

mainNN :: [Flag] -> [String] -> IO ()
mainNN _ ("matrix":file:_) = runNN $ MatrixFile file
mainNN _ ("coords":file:_) = runNN $ Euc2DFile file
mainNN _ _ = putStrLn $ usageInfo header options

runNN :: TSPFile -> IO ()
runNN file = do
        (n, dist) <- readProblemFunction file
	let path = TSP.NN.optimize dist n
--	let paths = map (findPath n dist) [1..n]
--	let path = minimum $ map (pathLen dist &&& id) paths
        putStrLn $ show (fst path) ++ " 0"
--	mapM_ (putStr . (++ " ") . show) . take n $ snd path
	mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd path
        putStrLn ""

mainACO :: [Flag] -> [String] -> IO ()
mainACO o ("matrix":file:_) = runACO o $ MatrixFile file
mainACO o ("coords":file:_) = runACO o $ Euc2DFile file
mainACO _ _ = putStrLn $ usageInfo header options

runACO :: [Flag] -> TSPFile -> IO ()
runACO opts file = do
  let conf n = foldl modConfig (defConfig n) opts
  minPath <- (\ (n, p) -> ACO.optimize (conf n) p) =<< readProblemMatrix file 
  putStrLn $ show (fst minPath) ++ " 0"
  mapM_ (putStr . (++ " ") . show . (+ (-1))) . tail $ snd minPath
  putStrLn ""

modConfig :: ConfigACO -> Flag -> ConfigACO
modConfig conf (FGen g) = conf {paramNGen = g}
modConfig conf (FAnts a) = conf {paramAGen = a}
modConfig conf FTwoOpt = conf {paramUse2Opt = True}
modConfig conf _ = conf
