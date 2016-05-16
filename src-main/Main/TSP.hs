{-# LANGUAGE ViewPatterns #-}

module Main.TSP where

import Control.Monad
import Control.Monad.Random
import System.Console.GetOpt

import ACO
import SA
import TSP
import TSP.NN
import qualified TSP.TwoOpt
import BB

import Main.Opts
import Main.Plot

getTSPfile :: [Flag] -> [String] -> TSP.Config
getTSPfile ((FCoords `elem`) -> True) (file:_) = TSP.defConfig $ Euc2DFile file
getTSPfile _ (file:_) = TSP.defConfig $ MatrixFile file
getTSPfile _ _ = error $ usageInfo header options

getTSPConf :: [Flag] -> [String] -> TSP.Config
getTSPConf opts args = foldl modConfigTSP (getTSPfile opts args) opts

saConfig :: SA.Config
saConfig = SA.Config 300000 0.99 10 7 10

printResult :: (Distance, Path) -> IO ()
printResult minPath = do
  print (fst minPath)
  mapM_ (putStr . (++ " ") . show . subtract 1) . tail $ snd minPath
  putStrLn ""

tspSA :: [Flag] -> [String] -> IO ()
tspSA opts args = do
  (n, distf) <- readProblemFunction $ getTSPConf opts args
  let conf = saConfig
  (minPath1, inf) <- evalRandIO $ SA.optimize
                     conf
                     (neighbours distf n)
                     (score distf $ [1..n] ++ [1], [1..n] ++ [1]) -- TODO random initial state
  when (FVerb `elem` opts) $ mapM_ putStrLn inf
  case fPlot opts of
    Nothing -> return ()
    Just file -> plotScore file (info conf) $ map ((read :: String -> Double) . head . words) inf
  let minPath = if FTwoOpt `elem` opts 
        then let twoO = TSP.TwoOpt.optimize distf $ snd minPath1 in
                          (sum $ zipWith (curry distf) twoO (tail twoO), twoO)
        else minPath1
  printResult minPath

tspNN :: [Flag] -> [String] -> IO ()
tspNN opts args = do
  (n, dist) <- readProblemFunction $ getTSPConf opts args
  let path = TSP.NN.optimize dist n
  let minPath = if FTwoOpt `elem` opts
        then let twoO = TSP.TwoOpt.optimize dist $ snd path in
                          (sum $ zipWith (curry dist) twoO (tail twoO), twoO)
        else path
  printResult minPath

tspACO :: [Flag] -> [String] -> IO ()
tspACO opts args = do
  let conf n = foldl modConfigACO (ACO.defConfig n) opts
  (minPath, inf) <- (\ (n, p) -> ACO.optimizeWithInfo (conf n) p) =<< readProblemMatrix (getTSPConf opts args) 
  when (FVerb `elem` opts) $ mapM_ print inf
  case fPlot opts of
    Nothing -> return ()
    Just file -> plotScore file 1 inf
  printResult minPath

tspBB :: [Flag] -> [String] -> IO ()
tspBB opts args = do
  (n, dist) <- readProblemFunction $ getTSPConf opts args
  let conf = foldl modConfigBB (BB.defConfig n) opts
  BB.optimize dist conf
