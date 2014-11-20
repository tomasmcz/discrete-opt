module Mravenci (optimize) where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Writer
import Data.Array.Unboxed
import qualified Data.IntSet as Set
import Data.List
--import Data.List.Stream
--import Prelude hiding (map, minimum, splitAt, sum, repeat, tail, take, zip)
import qualified Two_opt as T

type Vertex = Int
type Path = [Vertex]
type Size = Int
type Distance = Double
type Probability = Double
type Pheromon = Double
type Coefficient = Double
type DArray = UArray (Vertex, Vertex) Distance
type PArray = UArray (Vertex, Vertex) Pheromon
type CArray = UArray (Vertex, Vertex) Coefficient
type FDist = ((Vertex, Vertex) -> Distance)
type FPher = ((Vertex, Vertex) -> Pheromon)
type FCoef = ((Vertex, Vertex) -> Coefficient)

paramAGen = 32 --number of ants in one generation
paramNGen = 500 --number of generations
paramInitPh = 1e16 :: Pheromon --initial value of pheromones
paramAlpha = 1.0 :: Coefficient --Alpha parameter
paramBeta = 2.0 :: Coefficient --Beta parameter
paramEvRate = 0.001 :: Pheromon --evaporation rate
paramPBest = 0.0000005 :: Pheromon --PBest mmas parameter

getDistances :: Size -> ((Size, Size) -> Distance) -> DArray
getDistances n distf = listArray ((1, 1), (n, n)) [distf (a, b) | a <- [1..n], b <- [1..n]]

newPhrArray :: Size -> PArray
newPhrArray n = listArray ((1, 1), (n, n)) $ repeat paramInitPh

updatePheromones :: Pheromon -> Distance -> (Distance, Path) -> PArray -> PArray
updatePheromones pBestCoef minP (ln, bp) = bound . update . evap
	where	evap = amap ((1.0 - paramEvRate) *)

		update = flip (accum (+)) pathPairs 
		pathPairs = zip (zip bp (tail bp)) (repeat addC)
		addC = 10.0 / ln :: Pheromon

		bound = amap (min phMax . max phMin)
		phMax = 10.0 / (paramEvRate * minP) :: Pheromon
		phMin = phMax * pBestCoef

coefs :: Size -> FDist -> FPher -> FCoef
coefs n fDist fPher = (coefArr !) 
	where	coefArr = listArray ((1, 1), (n, n)) lst :: CArray
		lst = [val (x, y) | x <- [1..n], y <- [1..n]] 
		val (x, y) = (fPher (x, y) ** paramAlpha) / (fDist (x, y) ** paramBeta)

findPath :: Size -> FCoef -> Rand StdGen Path
findPath n coef = do
	origin <- getRandomR (1, n)
	let st = (Set.delete origin (Set.fromAscList [1..n]), origin)
	res <- execWriterT . flip evalStateT st . fix $ \ loop -> do
		(unv, last) <- get
		next <- lift . lift $ pickNext (\ x -> coef (last, x)) (Set.elems unv) 
		let unv' = Set.delete next unv
		put (unv', next)
		tell [next]
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
	res <- allPaths n (coefs n fDist fPher) paramAGen
	return . minimum . map (pathLen fDist &&& id) $ res

pathLen :: FDist -> Path -> Distance
pathLen fDist path = foldl' ((. fDist) . (+)) 0 (zip path (tail path))

generations :: Size -> DArray -> Rand StdGen [(Distance, Path)] 
generations n dist = do
	let nthRoot = paramPBest ** (1.0 / fromIntegral n)
	let pBestCoef = (1.0 - nthRoot) / (((fromIntegral n / 2.0) - 1.0) * nthRoot) :: Pheromon
	execWriterT . flip evalStateT (newPhrArray n, 0) . forever $ do
		(pher, mp) <- get
		bp <- lift . lift $ bestPath (dist !) (pher !) n 
--		let bp = use2opt (dist !) $ bestPath (dist !) (pher !) n 
		tell [bp]
		let minP = if mp == 0 then fst bp else min mp (fst bp)
		let newPher = updatePheromones pBestCoef minP bp
		put (newPher pher, minP)

use2opt dist (_, path) = let newPath = T.optimize dist path in (pathLen dist newPath, newPath)

optimize :: Size -> DArray -> IO (Distance, Path)
optimize n dist = do
	gens <- evalRandIO $ generations n dist 
	let minPath = minimum . take paramNGen $ gens
	--let minPath = use2opt (dist !) .  minimum . take paramNGen $ gens
	return minPath

