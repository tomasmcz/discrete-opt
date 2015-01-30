module Main.Opts where

import System.Console.GetOpt

import ACO (ConfigACO(..))
import SA (Config(..))

data Flag = FTwoOpt | FVersion | FHelp | FGen Int | FAnts Int | FCoords
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option ['t'] ["two-opt"] (NoArg FTwoOpt) "use 2-opt"
  , Option ['v','?'] ["version"] (NoArg FVersion) "show version number"
  , Option ['h'] ["help"] (NoArg FHelp) "display this usage info"
  , Option ['g'] ["generations"] (ReqArg (FGen . read) "N") "number of ACO generations"
  , Option ['a'] ["ants"] (ReqArg (FAnts . read) "N") "number of ants in one generation"
  , Option [] ["coords"] (NoArg FCoords) "coordinates file format fot TSP"
  ]

getOpts :: [String] -> IO ([Flag], [String])
getOpts argv  = case getOpt Permute options argv of
    (o, n, []) -> return (o,n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

header :: String
header = "Usage: discrete-opt <tsp|vrp> <aco|sa|nn> FILE [OPTION...]"

modConfig :: ConfigACO -> Flag -> ConfigACO
modConfig conf (FGen g) = conf {paramNGen = g}
modConfig conf (FAnts a) = conf {paramAGen = a}
modConfig conf FTwoOpt = conf {paramUse2Opt = True}
modConfig conf _ = conf

modConfigSA :: SA.Config -> Flag -> SA.Config
modConfigSA conf _ = conf