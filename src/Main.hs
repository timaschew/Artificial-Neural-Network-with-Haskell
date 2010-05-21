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
-- output
n3_1 = defaultNeuron { weight = [0.632, 0.952, 0.742] }

network = [[n1_1, n1_2, n1_3], [n2_1, n2_2, n2_3], [n3_1]]

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

-- calc delta for first neuron of calced outputLayer
calcOutputDelta outputLayer expected = makeDelta (n) delta
	where
	n = outputLayer !! 0
	s = (state n)
	delta = (expected - s) * s * (1 - s)

-- start step 3: backward pass

-- backpass algo steps:
-- 0) calculating deltas of output layer

-- 1) calculating weight deltas between outputlayer and previous layer
-- 2) calculating new weights
-- 3) calculating delta of previous layer

-- repeat step 1-3 until previous layer is inputlayer

-- calc recursivly weights between n and rn
-- @param n = neuron
-- @param w/weights - weights of neuron
-- @param rn = right neuron (using ONLY delta of its => TODO?)
-- @param result = recursion result
calcDeltaWeightsOfNeuron :: [Double] -> Neuron -> [Double] -> [Double] -> [Double]
calcDeltaWeightsOfNeuron [] rn [] result = result
calcDeltaWeightsOfNeuron (s:states) rn (w:deltaWeights) result = calcDeltaWeightsOfNeuron states rn deltaWeights (result ++ c:[])
	where
	-- formla: W(D)[2][1][1] = L * (D)[3][1] * N[2][1] + M * this(t-1)
	c = learnRate * (delta rn) * s + momentum * w

calcDeltaWeightsOfNeuron (s:states) rn [] result = calcDeltaWeightsOfNeuron states rn [] (result ++ c:[])
	where
	-- formla: W(D)[2][1][1] = L * (D)[3][1] * N[2][1]
	c = learnRate * (delta rn) * s
	-- Don't use momentum (not need for first learn iteration (no deltaWeights)

-- filters the layer of neurons only for state:
makeStateListOfLayer :: [Neuron] -> [Double] -> [Double]
makeStateListOfLayer [] result = result
makeStateListOfLayer (l:layer) result = makeStateListOfLayer layer (result ++ (state l):[])

-- create new neuron with calculated weight delta (previous layer (only states) also needed: leftLayer)
makeDeltaWeight :: [Neuron] -> Neuron -> [Neuron] -> Neuron
makeDeltaWeight leftLayer r result = newNeuron
	where
	leftLayerStates = makeStateListOfLayer leftLayer []	
	calcedWeights = calcDeltaWeightsOfNeuron leftLayerStates r (deltaWeight r) []
	newNeuron = makeDeltaWeights r calcedWeights

-- creates and return the rightLayer with calculated weight deltas
calcWeightDeltas :: [Neuron] -> [Neuron] -> [Neuron] -> [Neuron]
calcWeightDeltas leftLayer [] result = result
calcWeightDeltas leftLayer (r:rightLayer) result = calcWeightDeltas leftLayer rightLayer (result ++ c:[])
	where
	c = makeDeltaWeight leftLayer r []

-- update weights of given layer
updateWeights [] result = result
updateWeights (n:layer) result = updateWeights layer (result ++ (updateSingleWeight n (weight n) (deltaWeight n)):[])

-- caclulate new weight with the formula: W[2][1][1](t+1) = W[2][1][1](t) + WD[2][1][1]
updateSingleWeight :: Neuron -> [Double] -> [Double] -> Neuron
updateSingleWeight neuron w deltaW = setWeights neuron (zipWith (+) w deltaW)

-- expected values: input: (1,0,0) / output: 1
expectedValue = 10
deltaOutputNeuron = calcOutputDelta calcedOutput expectedValue

-- backpass step 1
backpropagation_step1 = calcWeightDeltas calcedHiddenLayer [deltaOutputNeuron] []

-- backpass step 2
backpropagation_step2 = updateWeights backpropagation_step1 []

main::IO()
main = do
	
	printState $ concat calcedNetwork
	putStr $ "output: "
	printState $ concat [calcedOutput]
