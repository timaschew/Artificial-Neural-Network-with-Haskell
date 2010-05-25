{--

TODO:

for difficult / complex things use Issues on Github
http://github.com/timaschew/Artificial-Neural-Network-with-Haskell/issues

- test genericTraining function
- use foldl instead of recursivly list operations
- error handling using network with layer length <3 
- use Haddock: http://www.haskell.org/haddock/
- interface between trainingdata file/parser and function calcError
- add more boilerplate code for (automated) testing 
	- topology parser
	- network parser
	- learning parser
	
QUESTIONs:
- avoid creating new list. use network in place instead? (calcLayer)

--}

module Main where
import Neuron
import Trainingdata
import Backpropagation
import Control.Monad
import Text.Printf

main::IO()
main = do
	
	--printState $ concat forwardedNetwork
	--putStr $ "output: "
	--printState $ concat [last forwardedNetwork]
	print goodNet
	
	
printState :: [Neuron] -> IO()
printState neuronList = do
	let stateList = map (\n -> state n) neuronList
	let result = zipWith (\state i ->  "n[" ++ show i ++ "].state = " ++ show state) stateList [1..]
	putStr $ unlines result

printNet :: Network -> IO()
printNet network = do
	let neuronList = concat network
	let result = zipWith (\neuron i ->  "n[" ++ show i ++ "] : " ++ show neuron) neuronList [1..]
	putStr $ unlines result


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
	stateStr = printf "%f" ((state n) :: Double)
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



-- #################################
-- Topology and Neuron Configuration
-- input
b1_0 = biasNeuron 
n1_1 = defaultNeuron
n1_2 = defaultNeuron
-- hidden
b2_0 = biasNeuron { weights = [0,0,0] }
n2_1 = defaultNeuron { weights = [0.033, -0.429, -0.938] }
n2_2 = defaultNeuron { weights = [0.763, -0.904, -0.330] }
-- output
n3_1 = defaultNeuron { weights = [0.632, 0.952, 0.742] }

network = [[b1_0, n1_1, n1_2], [b2_0, n2_1, n2_2], [n3_1]]
-- #################################

--
-- STARTING Forward Pass (doing manuel)
--
-- works with >=3 layers now.
inputLayer = head network
hiddenLayers = tail (init network)
hiddenLayer1 = hiddenLayers !! 0
outputLayer = last network

forwardedNetwork = [inputLayer] ++ forwardPass network []

--
-- STARTING calculate Error and Backward Pass
--
-- expected values: input: (1,0,0) / output: 10
-- define expected values manuely
expectedValue = 10
calcedOutput = last forwardedNetwork
calcedErrorOutputLayer = calcLayerError calcedOutput [expectedValue] []

-- updating forwardedNetwork
calcedErrorNetwork = (init forwardedNetwork) ++ [calcedErrorOutputLayer]

-- here: call backwardPass calcedErrorNetwork

-- start step 3a
-- manuel way
calcedHiddenLayer = forwardedNetwork !! 1
backpropagation_step3a = calcLayerDeltaWeigts calcedHiddenLayer calcedErrorOutputLayer []

-- start step 3b
-- manuel way
backpropagation_step3b = updateLayerWeights backpropagation_step3a []

-- start step 3b
-- manuel way
backpropagation_step3c = calcLayerDelta calcedHiddenLayer backpropagation_step3b [] 0


--
-- Start generic way
--

-- XOR trainingdata
inputValues = [[0,0],[0,1],[1,0],[1,1]]
outputValues = [[0],[1],[1],[0]]
tdata = Trainingdata 4 inputValues outputValues

-- HERE IT IS - The result from the generic backpropagation algorithm (only 4 learnsteps)
trainedNet = genericTraining network tdata 0

goodNet = trainNet network 8000 -- (5000 * 4 learnsteps)

-- set input layer with the given TrainData and call forwardPass
-- show only state of the neuron(s) of output layer
-- exmaple use case:
-- 1 start GHCi
-- 2 call 'goodNet' and wait - calculates the network
-- 3 call 'demo goodNet [1,0]' for showing result for the given input
demo :: Network -> TrainData -> [Double]
demo net inputData = result where
	inputted = setTrainToInputLayer net inputData
	forwarded = forwardPass inputted []
	result = makeStateListOfLayer (last forwarded) []

-- let train the net <steps> times
-- the function / algorithm is very slow :(
-- 5000 * 4 steps ~ 16 seconds (on a macbook 2GHz)
trainNet :: Network -> Int -> Network
trainNet net 0 = {-# SCC "trainNet" #-} net
trainNet net steps = trainNet trained (steps-1) where
	trained = genericTraining net tdata 0
	
	
