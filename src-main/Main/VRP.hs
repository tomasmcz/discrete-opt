{-# LANGUAGE ViewPatterns #-}

module Main.VRP where

import Control.Monad.Random
--import System.Console.GetOpt
import System.Random ()
import qualified Data.Map as M

import ACO
import SA
import VRP
import Main.Opts

saConfig :: SA.Config
saConfig = SA.Config 300000 0.99 100 7 100

vrpSA :: [Flag] -> [String] -> IO ()
vrpSA opts (file:_) = do
  (n, v, c, distF, demF) <- readProblemF $ file
  let initState = M.fromList . zip [1..] $ [1..n-1] ++ replicate (v - 1) 0
  let (initSc, initSol) = solve distF demF c initState
      config = foldl modConfigSA saConfig opts
  (solut, info) <- evalRandIO $ SA.optimize config (neighbour (solve distF demF c) (1, M.size initState)) (initSc, (initState, initSol))
  mapM_ putStrLn info
  let (solution, info2) = SA.descent (allNeigh (solve distF demF c)) solut
  mapM_ putStrLn info2
  putStrLn $ (show . fst $ solution) ++ " 0"
  mapM_ (putStrLn . unwords . map show . (0 :) . (++ [0])) (snd . snd $ solution)

vrpACO :: [Flag] -> [String] -> IO ()
vrpACO opts (file:_) = do
  (n, v, dist, p) <- readProblemM $ file
  let conf = (foldl modConfig (defConfig (n - 1)) opts) {penalty = p, originRandom = False, returnToOrigin = v - 1}
  minPath <- ACO.optimize conf dist
  putStrLn $ show (fst minPath) 
  mapM_ (putStr . (++ " ") . show) $ snd minPath
  putStrLn ""