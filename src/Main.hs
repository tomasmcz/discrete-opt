{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Console.GetOpt
import System.Environment

import MainACO
import MainNN

data Flag = FTwoOpt | FVersion | FHelp
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option "t" ["two-opt"] (NoArg FTwoOpt) "use 2-opt"
  , Option ['v','?'] ["version"] (NoArg FVersion) "show version number"
  , Option ['h'] ["help"] (NoArg FVersion) "display this usage info"
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
run opts ["aco", file] = mainACO opts file
run opts ["nn", file] = mainNN opts file
run _ _ = putStrLn $ usageInfo header options
