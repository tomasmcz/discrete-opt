{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Console.GetOpt
import System.Environment

import Main.ACO
import Main.NN
import Main.SA

data Flag = FTwoOpt | FVersion | FHelp
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option "t" ["two-opt"] (NoArg FTwoOpt) "use 2-opt"
  , Option ['v','?'] ["version"] (NoArg FVersion) "show version number"
  , Option ['h'] ["help"] (NoArg FHelp) "display this usage info"
  ]

getOpts :: [String] -> IO ([Flag], [String])
getOpts argv  = case getOpt Permute options argv of
    (o, n, []) -> return (o,n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

header :: String
header = "Usage: discrete-opt command file [OPTION...]"

main :: IO ()
main = do
  (opts, args) <- getOpts =<< getArgs  
  run opts args

run :: [Flag] -> [String] -> IO ()
run ((FHelp `elem`) -> True) _ = putStrLn $ usageInfo header options
run ((FVersion `elem`) -> True) _ = putStrLn $ "0.1.0.0"
run opts ["aco", file] = mainACO opts file
run opts ["nn", file] = mainNN opts file
run opts ["sa", file] = mainSA opts file
run _ _ = putStrLn $ usageInfo header options
