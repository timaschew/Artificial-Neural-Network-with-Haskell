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


main::IO()
main = do
	
	printState $ concat forwardedNetwork
	putStr $ "output: "
	printState $ concat [last forwardedNetwork]
	
	
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

-- #################################
-- Topology and Neuron Configuration
-- input
n1_1 = defaultNeuron
n1_2 = defaultNeuron
n1_3 = defaultNeuron
-- hidden
n2_1 = defaultNeuron { weights = [0.678, 0.211, -0.761] }
n2_2 = defaultNeuron { weights = [0.033, -0.429, -0.938] }
n2_3 = defaultNeuron { weights = [0.763, -0.904, -0.330] }
-- output
n3_1 = defaultNeuron { weights = [0.632, 0.952, 0.742] }

network = [[n1_1, n1_2, n1_3], [n2_1, n2_2, n2_3], [n3_1]]
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
inputValues = [[1,0,0],[0,1,0],[0,0,1],[0,0,0],[1,1,0], [0,1,1], [1,0,1], [1,1,1]]
outputValues = [[1],[1],[1],[0],[0],[0],[0],[1]]
tdata = Trainingdata 8 inputValues outputValues

-- HERE IT IS - The result from the generic backpropagation algorithm
trainedNet = genericTraining network tdata 0

timeTest = demo network 5000

-- forward the net (input values have to be set before) 
-- and show the state of the neuron(s) of the output layer
forwardAndShowResult net = result where
	forwarded = forwardPass net []
	result = makeStateListOfLayer (last forwarded) []

-- let train the net <steps> times
-- exmaple use case:
-- load Main in ghci and do type this into the interpreter
{--
	let goodNet = demo network 5000
	goodNet -- take a whlie
	-- define some networks with different inputLayer
	let i_100 = setTrainToInputLayer goodNet [1,0,0]
	let i_010 = setTrainToInputLayer goodNet [0,1,0]
	let i_001 = setTrainToInputLayer goodNet [0,0,1]
	let i_000 = setTrainToInputLayer goodNet [0,0,0]
	let i_110 = setTrainToInputLayer goodNet [1,1,0]
	let i_011 = setTrainToInputLayer goodNet [0,1,1]
	let i_101 = setTrainToInputLayer goodNet [1,0,1]
	let i_111 = setTrainToInputLayer goodNet [1,1,1]
	
	forwardAndShowResult i_010
	forwardAndShowResult i_111
	...
	
	-- WARNING
	-- currently the network will no train perfectly
	-- the topology configuration isn't usefull for the logic purpose
	-- other reason is, that the network has no bias, this will be implemented in future
	
--}
-- the function / algorithm is very slow :(
-- 5000 steps ~ 16 seconds (on a macbook 2GHz)
demo net 0 = net
demo net steps = demo trained (steps-1) where
	trained = genericTraining net tdata 0
	
	