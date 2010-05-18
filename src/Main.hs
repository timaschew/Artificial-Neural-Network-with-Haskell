{--

artificial neural network with backpropagation as teaching method.

backpropagation algo steps:
1) forward pass
2) calculate error
3) backward pass

topology:
- at least 1 hidden layer
- feedforward (no shortcuts, each neuron has a connection to each neuron of the next layer, without any back connections)

TODO:
- expected teaching values
- teaching steps count
- add more boilerplate code for (automated) testing
--}

module Main where
import Neuron

-- sigmoid function
sigmoidFunction :: (Floating a) => a -> a
sigmoidFunction x = 1 / (1 + (exp (-x)))

-- calculate state * weight + offset
-- use offset for calculation in previous recursion
-- @param [Neuron]:	Neuron list of a single layer
-- @param [Double]: Neuron weights (w.i) of layer n for a certain Neuron of layer n+1: 
--
--	[List of Neurons] ---- w.i ---->  [single Neuron] (layer n + 1)
--
-- @return Double: offset
-- @return Double: netinput (sum) for a certain Neuron of layer n + 1
calc :: [Neuron] -> [Double] -> Double -> Double
calc [] [] c = c
calc (n:ns) (x:xs) c = calc (ns) (xs) ((state n) * x + c)

-- TODO: Shoud  be tested, whether the result is correct
-- dynamic calculating from leftLayer to rightLayer
-- recursion: call calcLayer with leftLayer an rs and calcedFirstNeuron
-- calcedFirstNeuron: previousNeuronList (c) ++ current Neuron (calculated with Neuron constructor) as List
-- QUESTION: avoid creating new list. use network in place instead?
calcLayer :: [Neuron] -> [Neuron] -> [Neuron] -> [Neuron]
calcLayer leftLayer [] c = c
calcLayer leftLayer (r:rs) c = calcLayer leftLayer (rs) (c ++ (makeNeuron iSum sta (weight r)):[])
	where
	iSum = calc leftLayer (weight r) 0
	sta = sigmoidFunction (calc leftLayer (weight r) 0)


--
-- FORWARD PASS
--
-- TODO: test state values. Note: it returns a network without the input layer yet.
-- use this method to do the 1. algo step. Example: printNet forwardPass network [[]] 
-- @params: currentNet newNet
-- @return new network
forwardPass :: [[Neuron]] -> [[Neuron]] -> [[Neuron]]
forwardPass [] newNet = newNet
forwardPass (l:ls) newNet =  forwardPass ls (newNet ++ [calcLayer l nextLayer []])
	where 
	nextLayer | length ls == 0 = []
			  | otherwise = (head ls)


printState :: [Neuron] -> IO()
printState neuronList = do
	let stateList = map (\n -> state n) neuronList
	let result = zipWith (\state i ->  "n[" ++ show i ++ "].state = " ++ show state) stateList [1..]
	putStr $ unlines result

printNet :: [[Neuron]] -> IO()
printNet network = do
	let neuronList = concat network
	let result = zipWith (\neuron i ->  "n[" ++ show i ++ "] : " ++ show neuron) neuronList [1..]
	putStr $ unlines result


-- #################################
-- Backpropagation Configuration
learnRate = 0.350
momentum = 0.15

-- Topology and Neuron Configuration
-- input
n1_1 = defaultNeuron {state = 1}
n1_2 = defaultNeuron
n1_3 = defaultNeuron
-- hidden
n2_1 = defaultNeuron { weight = [0.678, 0.211, -0.761] }
n2_2 = defaultNeuron { weight = [0.033, -0.429, -0.938] }
n2_3 = defaultNeuron { weight = [0.763, -0.904, -0.330] }
n2_4 = defaultNeuron { weight = [0.223, 0.194, 0.189] }
-- output
n3_1 = defaultNeuron { weight = [0.632, 0.952, 0.742, -0.968] }

network = [[n1_1, n1_2, n1_3], [n2_1, n2_2, n2_3, n2_4], [n3_1]]

-- output of n3_1 should be = 0.705 
-- #################################

-- input output example: 2 x 2 x 1 topology
-- input: n1_1 = 1.000, n1_2 = 0.500
-- weights: 	n2_1 = [0.123, 0.234]
--		n2_2 = [0.987, .0876]
--		n3_1 = [0.264, 0.633]
-- output sould be:
-- n2_1 = 0.24
-- n2_2 = 1.425
-- n3_1 = 0.658

-- start step 1: forward pass

-- works with >=3 layers now. TODO: error handling for layer length <3
inputLayer = head network
hiddenLayers = tail (init network)
hiddenLayer1 = hiddenLayers !! 0
-- TODO: dont ignore other hiddenLayer, currently only first hidden layer works
outputLayer = last network

calcedHiddenLayer :: [Neuron]
calcedHiddenLayer = calcLayer inputLayer hiddenLayer1 []

calcedOutput :: [Neuron]
calcedOutput = calcLayer calcedHiddenLayer outputLayer []

calcedNetwork = [inputLayer, calcedHiddenLayer, calcedOutput]

-- start step 2: calculate error
-- manuel hardcoded static way

-- expected values: input: (1,0,0) / output: 1
expectedValue = 1
deltaOutputNeuron = calcOutputDelta calcedOutput expectedValue

-- calc delta for first neuron of calced outputLayer
-- TODO: check the result, maybe wrong
calcOutputDelta outputLayer expected = makeDelta (n) delta
	where
	n = outputLayer !! 0
	s = (state n)
	delta = (expected * s) * s * (1 - s)

main::IO()
main = do
	
	printState $ concat calcedNetwork
	putStr $ "output: "
	printState $ concat [calcedOutput]
