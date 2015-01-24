{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Console.GetOpt
import System.Environment

import Main.Opts
import Main.TSP
import Main.VRP

main :: IO ()
main = do
  (opts, args) <- getOpts =<< getArgs  
  run opts args

run :: [Flag] -> [String] -> IO ()
run ((FHelp `elem`) -> True) _ = putStrLn $ usageInfo header options
run ((FVersion `elem`) -> True) _ = putStrLn $ "0.1.0.0"
run opts ("tsp":"aco":args) = tspACO opts args
run opts ("tsp":"nn":args) = tspNN opts args
run opts ("tsp":"sa":args) = tspSA opts args
run opts ("vrp":"sa":args) = vrpSA opts args
run _ _ = putStrLn $ usageInfo header options
