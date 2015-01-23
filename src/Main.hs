{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Console.GetOpt
import System.Environment

import Main.TSP
import Main.Opts

main :: IO ()
main = do
  (opts, args) <- getOpts =<< getArgs  
  run opts args

run :: [Flag] -> [String] -> IO ()
run ((FHelp `elem`) -> True) _ = putStrLn $ usageInfo header options
run ((FVersion `elem`) -> True) _ = putStrLn $ "0.1.0.0"
run opts ("aco":args) = mainACO opts args
run opts ("nn":args) = mainNN opts args
run opts ("sa":args) = mainSA opts args
run _ _ = putStrLn $ usageInfo header options
