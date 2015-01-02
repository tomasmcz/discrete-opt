module Main.Opts where

import System.Console.GetOpt

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


