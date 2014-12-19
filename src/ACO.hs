{- |
Copyright    : 2014 Tomáš Musil
License      : BSD-3
Stability    : experimental
Portability  : portable 

Ant Colony Optimization.

-}

module ACO 
  (
  -- * Types
    Pheromon
  , Coefficient
  -- * Configuration
  , ConfigACO(..)
  , defConfig
  -- * Optimization
  , optimize
  ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Writer
import Data.Array.Unboxed
import qualified Data.IntSet as Set
import Data.List (foldl')
--import Data.List.Stream
--import Prelude hiding (map, minimum, splitAt, sum, repeat, tail, take, zip)

import TSP hiding (CArray)
import qualified TSP.TwoOpt as Topt

type Probability = Double
type Pheromon = Double
type Coefficient = Double
type PArray = UArray (Vertex, Vertex) Pheromon
type CArray = UArray (Vertex, Vertex) Coefficient
type FPher = ((Vertex, Vertex) -> Pheromon)
type FCoef = ((Vertex, Vertex) -> Coefficient)

data ConfigACO = ConfigACO 
  { paramAGen :: Int -- ^ number of ants in one generation
  , paramNGen :: Int -- ^ number of generations
  , paramInitPh :: Pheromon -- ^ initial value of pheromones
  , paramAlpha  :: Coefficient -- ^ Alpha parameter
  , paramBeta   :: Coefficient -- ^ Beta parameter
  , paramEvRate :: Pheromon -- ^ evaporation rate
  , paramPBest  :: Pheromon -- ^ PBest mmas parameter
  , paramUse2Opt :: Bool -- ^ use 2-Opt heuristic
  }

-- | Default configuration.
defConfig :: ConfigACO
defConfig = ConfigACO 32 
                      500
                      1e16
                      1.0 
                      2.0 
                      0.001
                      0.0000005 
                      False

newPhrArray :: Size -> PArray
newPhrArray n = listArray ((1, 1), (n, n)) $ repeat (paramInitPh defConfig)

updatePheromones :: Pheromon -> Distance -> (Distance, Path) -> PArray -> PArray
updatePheromones pBestCoef minP (ln, bp) = bound . update . evap
	where	evap = amap ((1.0 - paramEvRate defConfig) *)

		update = flip (accum (+)) pathPairs 
		pathPairs = zip (zip bp (tail bp)) (repeat addC)
		addC = 10.0 / ln :: Pheromon

		bound = amap (min phMax . max phMin)
		phMax = 10.0 / (paramEvRate defConfig * minP) :: Pheromon
		phMin = phMax * pBestCoef

coefs :: Size -> FDist -> FPher -> FCoef
coefs n fDist fPher = (coefArr !) 
	where	coefArr = listArray ((1, 1), (n, n)) lst :: CArray
		lst = [val (x, y) | x <- [1..n], y <- [1..n]] 
		val (x, y) = (fPher (x, y) ** paramAlpha defConfig) / (fDist (x, y) ** paramBeta defConfig)  

findPath :: Size -> FCoef -> Rand StdGen Path
findPath n coef = do
	origin <- getRandomR (1, n)
	let st = (Set.delete origin (Set.fromAscList [1..n]), origin)
	res <- execWriterT . flip evalStateT st . fix $ \ loop -> do
		(unv, lst) <- get
		nxt <- lift . lift $ pickNext (\ x -> coef (lst, x)) (Set.elems unv) 
		let unv' = Set.delete nxt unv
		put (unv', nxt)
		tell [nxt]
		unless (Set.null unv') loop 
	return $ (origin:res) ++ [origin]

pickNext :: (Vertex -> Coefficient) -> [Vertex] -> Rand StdGen Vertex
pickNext coef unv = do
	let list = map (coef &&& id) unv :: [(Probability, Vertex)]
	let norm = foldl' ((. fst) . (+)) 0 list
	r <- (* norm) <$> getRandom
	return . flip evalState (0, list) . fix $ \ loop -> do
		(s,(p,v):ts) <- get
		let ns = s + p
		put (ns, ts)
		if r <= ns 
			then return v
			else loop

allPaths :: Size -> FCoef -> Int -> Rand StdGen [Path]
allPaths n coef s = execWriterT . flip evalStateT s . fix $ \ loop -> do
	i <- get
	path <- lift . lift $ findPath n coef
	tell [path]
	put $ i - 1
	unless (i == 1) loop

bestPath :: FDist -> FPher -> Size -> Rand StdGen (Distance, Path)
bestPath fDist fPher n = do
	res <- allPaths n (coefs n fDist fPher) $ paramAGen defConfig
	return . minimum . map (pathLen fDist &&& id) $ res

pathLen :: FDist -> Path -> Distance
pathLen fDist path = foldl' ((. fDist) . (+)) 0 (zip path (tail path))

generations :: ConfigACO -> Size -> DArray -> Rand StdGen [(Distance, Path)] 
generations config n dist = do
	let nthRoot = paramPBest config ** (1.0 / fromIntegral n)
	let pBestCoef = (1.0 - nthRoot) / (((fromIntegral n / 2.0) - 1.0) * nthRoot) :: Pheromon
	execWriterT . flip evalStateT (newPhrArray n, 0) . forever $ do
		(pher, mp) <- get
                let heuristic = if paramUse2Opt config then use2opt (dist !) else id
		bp <- lift . lift $ heuristic <$> bestPath (dist !) (pher !) n 
		tell [bp]
		let minP = if mp == 0 then fst bp else min mp (fst bp)
		let newPher = updatePheromones pBestCoef minP bp
		put (newPher pher, minP)

use2opt :: FDist -> (Distance, Path) -> (Distance, Path)
use2opt dist (_, path) = let newPath = Topt.optimize dist path in (pathLen dist newPath, newPath)

optimize :: ConfigACO -> Size -> DArray -> IO (Distance, Path)
optimize config n dist = do
	gens <- evalRandIO $ generations config n dist 
	return . minimum . take (paramNGen defConfig) $ gens

