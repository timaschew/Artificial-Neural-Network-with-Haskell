{--

TODO:  clean up the print functions

--}

module Utils where

import Neuron
import Trainingdata
import Backpropagation
import TopologyParser
import TraindataParser
import GraphicInterface
import Config
import Control.Monad
import System.Environment
import Text.Printf
import System.Directory
import Data.List

{----------------------------------------------------------------------
	Print Utils
----------------------------------------------------------------------}
showError net = do
	let outputLayer = last net
	let l = map delta outputLayer
	putStr "["
	mapM_ (\v -> printf "%.3f " v) l
	putStr "]"
	putStrLn ""

showOutput :: Network -> TrainData -> IO ()
showOutput net  train = do
	let l = work net train
	putStr "["
	mapM_ (\v -> printf "%.3f " v) l
	putStr "]"
	putStrLn ""
	
	
-- print states of a layer
printState :: [Neuron] -> IO()
printState neuronList = do
	let stateList = map (\n -> state n) neuronList
	let result = zipWith (\state i ->  "n[" ++ show i ++ "].state = " ++ show state) stateList [1..]
	putStr $ unlines result

-- print network (neuron index and state)
printNet :: Network -> IO()
printNet network = do
	let neuronList = concat network
	let result = zipWith (\neuron i ->  "n[" ++ show i ++ "] : " ++ show neuron) neuronList [1..]
	putStr $ unlines result


-- alternative pretty print
-- print list of strings (one element for each line)
printPretty net = splitAndNewline where
	result = prettyP net 1 []
	splitAndNewline = putStrLn (unlines result)

-- print neuron indices and state as string list
prettyP :: Network -> Int -> [String] -> [String]
prettyP [] row c = c
prettyP (l:net) row c = prettyP net (row+1) tmp where
	neurons = printNeuronLayers l row 1 []
	states = printStateLayers l ""
	tmp = c ++ [neurons] ++ [states] ++ [""]

-- print indices for a layer
printNeuronLayers :: [Neuron] -> Int -> Int -> String -> String
printNeuronLayers [] row col c = c
printNeuronLayers (n:l) row col c =  printNeuronLayers l row (col+1) tmp where
	updated = printNeuron n row col
	tmp = c ++ updated
-- print neuron index
printNeuron :: Neuron -> Int -> Int -> String
printNeuron n row col = printed  where
	i	| (bias n) == True = 0
		| otherwise = col
	printed1l = printf "N[%d][%d]     " (row::Int) (i)
	printed = printed1l

-- print state for a layer
printStateLayers :: [Neuron] -> String -> String
printStateLayers []  c = c
printStateLayers (n:l) c =  printStateLayers l tmp where
	updated = printNeuronState n
	tmp = c ++ updated
	
-- print neuron state	
printNeuronState :: Neuron -> String
printNeuronState n = printed  where
	printed1l = printf "%f         " (state n)
	printed = printed1l
	
-- alternative pretty print END


--
-- pretty print method
--
prettyPrint :: Network -> IO()
prettyPrint network = do
	mapM_ putStr (buildPrettyStr network)


buildPrettyStr :: Network -> [String]
buildPrettyStr net = result
	where
	list = buildLayerStr 1 net []
	max = getMaxLayerSize list 0
	result = concateAllNeuron max list []


-- returns: 
buildLayerStr :: Int -> Network -> [[String]] -> [[String]]
buildLayerStr curIndex [] res = res
buildLayerStr curIndex (l:layerList) res = buildLayerStr nextIndex layerList tmp
	where
	nextIndex = curIndex + 1
	-- check if first neuron is a bias or not
	realNeuronIndex	| (bias (head l)) == True = 0
			| otherwise = 1
	tmp = res ++ (buildNeuronStr curIndex realNeuronIndex l [] [])


buildNeuronStr :: Int -> Int -> [Neuron] -> [String] -> [String] -> [[String]]
buildNeuronStr i j [] r1 r2 = [r1] ++ [r2]
buildNeuronStr layerIdx neuronIdx (n:nList) r1 r2 = buildNeuronStr layerIdx nextIdx nList tmp1 tmp2
	where
	nextIdx = neuronIdx + 1
	neuronStr = printf "N[%d][%d]" (layerIdx::Int) (neuronIdx::Int)
	stateStr = printf "%.3f" ((state n) :: Double)
	tmp1 = r1 ++ [neuronStr]
	tmp2 = r2 ++ [stateStr]


getMaxLayerSize :: [[String]] -> Int -> Int
getMaxLayerSize [] max = max
getMaxLayerSize (curList:listOfList) max = getMaxLayerSize listOfList curMax
	where
	curMax | length curList > max = length curList
		   | otherwise = max


concateAllNeuron :: Int -> [[String]] -> [String] -> [String]
concateAllNeuron max [] res = res
concateAllNeuron max (l:lists) res = concateAllNeuron max restList tmp
	where
	restList | length lists > 0 = tail lists
			 | otherwise = []
	headList | length lists > 0 = head lists
			 | otherwise = []
	tmp = res ++ (concateNeuronStr max l headList)

concateNeuronStr :: Int -> [String] -> [String] -> [String]
concateNeuronStr maxNeurons nStrList sStrList = result
	where
	-- needed for calcinc the spaceL if this layer has less neurons
	maxDiff = maxNeurons - length nStrList
	
	maxDiffHalf | (maxDiff `mod` 2) == 0 = (maxDiff `div` 2)
				| otherwise = (maxDiff + 1) `div` 2
	
	-- spacer for the first neuron on the left side
	spacerL | maxDiff == 1 = buildSpacer 7
			| maxDiff > 0 && maxDiff `mod` 2 == 0 = buildSpacer (maxDiffHalf * 12)
			| maxDiff > 0 && maxDiff `mod` 2 == 1 = buildSpacer (maxDiffHalf * 9)
			| otherwise = buildSpacer 0
	
	-- spacer between 2 neurons
	spacerN = buildSpacer 5
	
	-- needet for extra spaces if state line below is shorter. 2nd map: length of neuron string 
	spacerS = zipWith (\n str -> buildSpacer (n - length str)) (map (\n -> length n) nStrList) sStrList
			
	-- row1: neurons, row2: states of neurons
	row1 = spacerL ++ concat (map (\n -> n ++ spacerN) nStrList) ++ "\n"
	row2 = spacerL ++ concat (zipWith (\sp n -> n ++ spacerN ++ sp) spacerS sStrList) ++ "\n\n"
	result = [(row1 ++ row2)]

buildSpacer :: Int -> String
buildSpacer size = concat (replicate size " ")


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
--initNetworkFromTdata ioTdata = do
	--tdata <- ioTdata
	let inputLen = length $ head (inputs tdata)		-- get size of one input
	let outputLen = length $ head (outputs tdata)	-- get size of one output
	let hiddenLen | inputLen > 30 = 15	-- max
				  | inputLen <= 2 = 2	-- min
				  | otherwise = ceiling (fromIntegral inputLen / 2)
	initNetwork (show inputLen ++ "b\n" ++ show hiddenLen ++ "b\n" ++ show outputLen ++ "\n")

-- generates a network from input string
initNetwork :: String -> IO Network
initNetwork input = do
	--input <- readFile filename
	randNums <- mapM (\x -> getRandNum) [1..countNeededRandNums input]
	let net = getTopology input randNums
	; return net

	
initTraindata :: String -> IO Trainingdata
initTraindata filename = do
	input <- readFile filename
	let tdata = getTrainingdata input
	; return tdata

-- let train the net <steps> times
-- the function / algorithm is very slow :(
-- 5000 * 4 steps ~ 16 seconds (on a macbook 2GHz)
trainNet :: Network -> Trainingdata -> Int -> Network
trainNet net tdata 0 = {-# SCC "trainNet" #-} net
trainNet net tdata steps = trainNet trained tdata (steps-1) where
	trained = genericTraining net tdata 0
	

-- how to use:
-- call 'work goodNet [1,0]' for showing result for the given input
work :: Network -> TrainData -> [Double]
work net inputData = result where
	inputted = setTrainToInputLayer net inputData
	forwarded = forwardPass inputted []
	result = makeStateListOfLayer (last forwarded) []


dec2bin = map i2c . reverse . unfoldr decomp
    where decomp n = if n == 0 then Nothing else Just(n `mod` 2, n `div` 2)
          i2c i = if i == 0 then '0' else '1'
          
dec2binLeading :: Int -> Int -> [String]
dec2binLeading x num = tmp where
	y = dec2bin x
	repList = repeat 0
	remainingNumbers = num - (length y)
	list1 = take remainingNumbers repList
	list2 = map int2Str list1
	list3 = map makeString y
	tmp 	| (length y <= num) = list2 ++ list3
		| otherwise = list2
		
int2Str x = show x

makeString :: Char -> String
makeString x = [x]


-- DB function - save weights

saveWeights net "" = do
	writeFile "/dev/null" "nothing"
saveWeights net fn = do
	let s = net2weightsString (tail net) (head net) []
	let sizeList = ("# " ++ unwords (map show (map length net)))
	writeFile fn (unlines ([sizeList] ++ s))

net2weightsString :: Network -> [Neuron] -> [String] -> [String]
net2weightsString [] prevLayer c = c
net2weightsString (l:rest) prevLayer c = net2weightsString rest l tmp where
	prevNeurons = length prevLayer
	weights = neurons2weightsString l []
	tmp = c ++ ["# " ++ (show prevNeurons) ++ " " ++ (show (length l))] ++ weights

neurons2weightsString [] c = c
neurons2weightsString (n:layer) c = neurons2weightsString layer tmp where
	tmp = c ++ [doubleList2String (weights n)]


doubleList2String l = tmp where
	sl = map show l
	tmp = unwords sl
	
	
-- DB function - load weights

--loadWeights :: String -> IO Network
loadWeights fn = do
	input <- readFile fn
	let inp = lines input
	let s = head inp -- 10 3 => 10 input neurons
	let inputSize = readI ((words s) !! 0)
	putStrLn ("" ++ (show inputSize))
	--let r = string2Net inp
	--return ""
	
--string2Net inp = tmp where
	
readI x = read x :: Int


-- generates a Trainingsdata from the ppm of the given directory
dirToTrainData :: String -> IO Trainingdata
dirToTrainData path = do
	inputValues <- getPPMInput path
	let outputValues = getOutputMatrix (length inputValues)
	let tdata = Trainingdata (length inputValues) inputValues outputValues
	return tdata

	
-- returns a list of inputvalues from the given ppm folder.
-- reads every ppm and generates its inputValues from the pixels values
getPPMInput :: String -> IO [[Double]]
getPPMInput path = do
	let fullPath = dataPath ++ path
	nameList <- getPgmList fullPath
	fileList <- mapM readPPMFile (map (\x -> fullPath ++ x) nameList)
	
	return fileList


-- scans a directory for ppm files and returns a list of their filenames
getPgmList :: String -> IO [String]
getPgmList path = do
	dirContent <- getDirectoryContents path
	let pgmList = filter (isSuffixOf ".pgm") dirContent
	return pgmList
	
-- returns the output values list (n x n identity matrix)
getOutputMatrix :: Int -> [[Double]]
getOutputMatrix n = map (\x-> map (\y-> if y == x then 1.0 else 0.0) [1..n]) [1..n]
