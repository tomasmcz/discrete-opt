module Main.Opts where

import System.Console.GetOpt

import ACO (Config(..))
import SA (Config(..))
import BB (Config(..))
import Problems.TSP as TSP (Config(..), DistConf(..))

data Flag = FTwoOpt | FVersion | FHelp | FGen Int | FAnts Int | FCoords | FVerb | FPlot FilePath | FFin Double | FOpts | FEst Double | FREuc
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option ['t'] ["two-opt"] (NoArg FTwoOpt) "use 2-opt"
  , Option ['v','?'] ["version"] (NoArg FVersion) "show version number"
  , Option ['h'] ["help"] (NoArg FHelp) "display this usage info"
  , Option ['g'] ["generations"] (ReqArg (FGen . read) "N") "number of ACO generations"
  , Option ['a'] ["ants"] (ReqArg (FAnts . read) "N") "number of ants in one generation"
  , Option [] ["coords"] (NoArg FCoords) "coordinates file format for TSP"
  , Option [] ["verb"] (NoArg FVerb) "verbose"
  , Option ['p'] ["plot"] (ReqArg FPlot "FILE") "plot score"
  , Option [] ["fin"] (ReqArg (FFin . read) "D") "final temperature for SA"
  , Option [] ["est"] (ReqArg (FEst . read) "D") "initial estimate for BB"
  , Option [] ["reuc"] (NoArg FREuc) "round Euclidean distance to nearest integer"
  , Option [] ["list-options"] (NoArg FOpts) "list options"
  ]

getOpts :: [String] -> IO ([Flag], [String])
getOpts argv  = case getOpt Permute options argv of
    (o, n, []) -> return (o,n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

listOpts :: [OptDescr Flag] -> IO ()
listOpts = mapM_ (\(Option _ [o] _ _) -> putStrLn $ "--" ++ o)

header :: String
header = "Usage: discrete-opt <tsp|vrp> <aco|sa|nn> FILE [OPTION...]"

modConfigTSP :: TSP.Config -> Flag -> TSP.Config
modConfigTSP conf FREuc = conf {distanceC = Euc2DRounded}
modConfigTSP conf _ = conf

modConfigACO :: ACO.Config -> Flag -> ACO.Config
modConfigACO conf (FGen g) = conf {paramNGen = g}
modConfigACO conf (FAnts a) = conf {paramAGen = a}
modConfigACO conf FTwoOpt = conf {paramUse2Opt = True}
modConfigACO conf _ = conf

modConfigSA :: SA.Config -> Flag -> SA.Config
modConfigSA conf (FFin t) = conf {finalTemp = t}
modConfigSA conf _ = conf

modConfigBB :: BB.Config -> Flag -> BB.Config
modConfigBB conf (FEst e) = conf {initEst = Just e}
modConfigBB conf _ = conf

fPlot :: [Flag] -> Maybe FilePath
fPlot (FPlot f:_) = Just f
fPlot (_:fs) = fPlot fs
fPlot [] = Nothing
