module Utils where

import Neuron
import Trainingdata
import Backpropagation
import TopologyParser
import TraindataParser
import GraphicInterface
import Control.Monad
import System.Environment
import Text.Printf
import System.Directory
import Data.List

dataPath = "../data/"

{----------------------------------------------------------------------
	Print Utils
----------------------------------------------------------------------}
-- show delta (quadratic error) for all output neurons
showError :: Network -> IO ()
showError net = do
	let outputLayer = last net
	let l = map delta outputLayer
	putStr "["
	mapM_ (\v -> printf "%.3f " v) l
	putStr "]"
	putStrLn ""

-- print output for traindata with the given network
showOutput :: Network -> TrainData -> IO ()
showOutput net  train = do
	let l = work net train
	putStr "["
	mapM_ (\v -> printf "%.3f " v) l
	putStr "]"
	putStrLn ""

showOutputFor net path = do
	tin <- readPPMFile path
	showOutput net tin
	
showNumsOutput net path = do
	mapM_ (\x -> do
		putStr ("[" ++ show x ++ "]: ")
		showOutputFor net (dataPath ++ path ++ show x ++ ".pgm")) [0..9]
	

-- print network (neuron index and state)
printNet :: Network -> IO()
printNet network = do
	let neuronList = concat network
	let result = zipWith (\neuron i ->  "n[" ++ show i ++ "] : " ++ show neuron) neuronList [1..]
	putStr $ unlines result

{----------------------------------------------------------------------
	Helper Methods
----------------------------------------------------------------------}
-- wrapps initNetwork to generate the network from file instead from string.
initNetworkFromFile :: String -> IO Network
initNetworkFromFile filename = do
	input <- readFile filename
	initNetwork input

-- The hiddenlayer is set automatically to half size of input length but max 15 neurons and at least 2, bias set default
initNetworkFromTdata :: Trainingdata -> IO Network
initNetworkFromTdata tdata = do
	let inputLen = length $ head (inputs tdata)		-- get size of one input
	let outputLen = length $ head (outputs tdata)	-- get size of one output
	let hiddenLen | inputLen > 30 = 15	-- max
				  | inputLen <= 2 = 2	-- min
				  | otherwise = ceiling (fromIntegral inputLen / 2)
	initNetwork (show inputLen ++ "b\n" ++ show hiddenLen ++ "b\n" ++ show outputLen ++ "\n")

-- generates a network from input string
initNetwork :: String -> IO Network
initNetwork input = do
	randNums <- mapM (\x -> getRandNum) [1..countNeededRandNums input]
	let net = getTopology input randNums
	; return net

genTopologyStr :: Trainingdata -> Bool -> Bool -> Int -> String
genTopologyStr tdata iBias hBias hiddenLen = do
	let inputLen = length $ head (inputs tdata)		-- get size of one input
	let outputLen = length $ head (outputs tdata)	-- get size of one output
	let b1  | iBias == True = "b"
			| otherwise = ""
	let b2  | hBias == True = "b"
			| otherwise = ""
	(show inputLen ++ b1 ++ "\n" ++ show hiddenLen ++ b2 ++ "\n" ++ show outputLen ++ "\n")
	
initTraindata :: String -> IO Trainingdata
initTraindata filename = do
	input <- readFile filename
	let tdata = getTrainingdata input
	; return tdata

-- let train the net <steps> times
-- the function / algorithm is very slow :(
-- 5000 * 4 steps ~ 16 seconds (on a macbook 2GHz)
trainNet :: Network -> Trainingdata -> Int -> Double -> Double -> Network
trainNet net tdata 0 _ _ = net
trainNet net tdata steps momentum learnRate = trainNet (seq trained trained) tdata (steps-1) momentum learnRate where
	trained = genericTraining net tdata 0 momentum learnRate
	notNeed = seq trained 1

-- how to use:
-- call 'work goodNet [1,0]' for showing result for the given input
work :: Network -> TrainData -> [Double]
work net inputData = result where
	inputted = setTrainToInputLayer net inputData
	forwarded = forwardPass inputted
	result = makeStateListOfLayer (last forwarded)

readI x = read x :: Int

-- generates a Trainingsdata from the ppm of the given directory
dirToTrainData :: String -> IO Trainingdata
dirToTrainData path = do
	inputValues <- getPPMInput path

	let outputValues = getOuputMatrixForMultipleDataset (length inputValues) 10
	
	let tdata = Trainingdata (length inputValues) inputValues outputValues
	return tdata


-- returns the output values list (n x n identity matrix)
getOutputMatrix :: Int -> [[Double]]
getOutputMatrix n = map (\x-> map (\y-> if y == x then 1.0 else 0.0) [1..n]) [1..n]

getOuputMatrixForMultipleDataset :: Int -> Int -> [[Double]]
getOuputMatrixForMultipleDataset size n = multiple where
	output = getOutputMatrix n
	m = (div size n) -- 6 datasets with each 10 fonts = 60, m = 60 / 10 = 6
	nTimes = take m (repeat output)
	multiple = concat nTimes
	
