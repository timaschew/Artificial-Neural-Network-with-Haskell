{--

artificial neural network with backpropagation as teaching method.

backpropagation algo steps:
1) forward pass
2) calculate error
3) backward pass
	3a) calculate weight deltas between layer N and layer N-1
	3b) calculate new weights
	3c) calculate delta of previous layer
	
repeat steps 3a - 3c and slide up the layers until layer N-1 == input layer
repeat step 3 <learn steps> times

############################################
## variable names convention in functions ##
############################################
l/layer = layer (list of neurons)
ll = left layer
rl = right layer
n = neuron (or rn = right neuron, ln = left neuron)
c = result from previous recursion
ws = list of weights
dws = list of delta weights
st = list of states

How-To-Read
<step_number> = <function_name> = <description>
	{ <function_called_by_function_above(nested) { ... }}

1 = forwardPass = calculating whole network (change and update X+1 in calcLayer function)
	{ calcLayer = calculating states between layer X and X+1 (begin at intput layer)
		{ calcNeuron = calculating state between layer X and one neuron of layer X+1 }}
		
2 = calcError = calculate quadratic error between real output and expected output

3 = backwardPass = calculate whole network (change and update layer like forwardPass)
3a	{ calcLayerDeltaWeigts = calculate weight-deltas between layer N and N-1
		{ calcNeuronDeltaWeights = calculate weight-deltas between one neuron N1 of layer N and whole layer N-1
			{ calcDeltaWeight = calculate formula for neuron N1 }}
3b	  updateLayerWeights = update weights of a layer X
	  	{ updateNeuronWeights = update weights of one neuron of layer X}
3c	  calcLayerDelta = calculate delta / error for layer X (like step 2)
	  	{ calcNeuronDelta = calculate delta / error for one neuron of layer X }}

TODO:

for difficult / complex things use Issues on Github
http://github.com/timaschew/Artificial-Neural-Network-with-Haskell/issues

- use foldl instead of recursivly list operations
- reduce function calls with long parameters (use temp calculations)
- fix forwardPass function (call updated nextLayer in recurision) and fix infinite loop
- add backwardPass function: inside it call steps: 3a, 3b, 3c
- add step 3c functions
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

main::IO()
main = do
	
	printState $ concat calcedNetwork
	putStr $ "output: "
	printState $ concat [calcedOutput]
	
	
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
n2_1 = defaultNeuron { weights = [0.678, 0.211, -0.761] }
n2_2 = defaultNeuron { weights = [0.033, -0.429, -0.938] }
n2_3 = defaultNeuron { weights = [0.763, -0.904, -0.330] }
-- output
n3_1 = defaultNeuron { weights = [0.632, 0.952, 0.742] }

network = [[n1_1, n1_2, n1_3], [n2_1, n2_2, n2_3], [n3_1]]
-- #################################

--
-- STARTING Forward Pass (currently forward pass dont work, doing manuel)
--
-- works with >=3 layers now.
inputLayer = head network
hiddenLayers = tail (init network)
hiddenLayer1 = hiddenLayers !! 0
outputLayer = last network

calcedHiddenLayer :: [Neuron]
calcedHiddenLayer = calcLayer inputLayer hiddenLayer1 []

calcedOutput :: [Neuron]
calcedOutput = calcLayer calcedHiddenLayer outputLayer []

calcedNetwork = [inputLayer, calcedHiddenLayer, calcedOutput]

--
-- STARTING calculate Error and Backward Pass (currently: manuel for single layers)
--
-- expected values: input: (1,0,0) / output: 10
expectedValue = 10
deltaOutputNeuron = calcError calcedOutput expectedValue

-- start step 3a
backpropagation_step3a = calcLayerDeltaWeigts calcedHiddenLayer [deltaOutputNeuron] []

-- start step 3b
backpropagation_step3b = updateLayerWeights backpropagation_step3a []



--
-- FORWARD PASS (step 1)
--
-- use this method to do the 1. algo step. Example: printNet forwardPass network [[]] 
-- @params: currentNet newNet
-- @return new network
forwardPass :: [[Neuron]] -> [[Neuron]] -> [[Neuron]]
forwardPass [] newNet = newNet
forwardPass (l:ls) newNet =  forwardPass ls (newNet ++ [calcLayer l nextLayer []])
	where 
	nextLayer | length ls == 0 = []
			  | otherwise = (head ls)

-- dynamic calculating from leftLayer to rightLayer
-- recursion: call calcLayer with leftLayer an rs and calcedFirstNeuron
-- calcedFirstNeuron: previousNeuronList (c) ++ current Neuron (calculated with Neuron constructor) as List
calcLayer :: [Neuron] -> [Neuron] -> [Neuron] -> [Neuron]
calcLayer ll [] c = c
calcLayer ll (r:rl) c = calcLayer ll (rl) tmp where 
	v_inputSum = calcNeuron ll (weights r) 0
	v_state = sigmoidFunction (calcNeuron ll (weights r) 0)
	updatedNeuron = makeNeuron v_inputSum v_state (weights r)
	tmp = c ++ [updatedNeuron]

-- calculate state * weight + offset
-- use offset for calculation in previous recursion
-- @param [Neuron]:	Neuron list of a single layer
-- @param [Double]: Neuron weights (w.i) of layer n for a certain Neuron of layer n+1: 
--
--	[List of Neurons] ---- w.i ---->  [single Neuron] (layer n + 1)
--
-- @return Double: offset
-- @return Double: netinput (sum) for a certain Neuron of layer n + 1
calcNeuron :: [Neuron] -> [Double] -> Double -> Double
calcNeuron [] [] c = c
calcNeuron (s:states) (w:weights) c = calcNeuron (states) (weights) tmp where
	v_inputSum = (state s) * w
	tmp = v_inputSum + c -- 

--
-- CALCULATE ERROR (step 2)
--
-- calc delta for only the one and first neuron of calced outputLayer
calcError outputLayer expected = setDelta (n) delta where
	n = outputLayer !! 0 -- only first neuron
	s = (state n)
	delta = (expected - s) * s * (1 - s)

--
-- BACKWARD PASS (step 3a)
--
-- helper function for step 3a
-- input: list of neurons (list); output: list of state values from the neurons
makeStateListOfLayer :: [Neuron] -> [Double] -> [Double]
makeStateListOfLayer [] c = c
makeStateListOfLayer (n:layer) c = makeStateListOfLayer layer (c ++ [(state n)])

-- creates and return the rightLayer with calculated weight deltas
calcLayerDeltaWeigts :: [Neuron] -> [Neuron] -> [Neuron] -> [Neuron]
calcLayerDeltaWeigts ll [] c = c
calcLayerDeltaWeigts ll (n:rl) c = calcLayerDeltaWeigts ll rl tmp where
	updatedNeuron = calcNeuronDeltaWeights ll n []
	tmp = c ++ [updatedNeuron]

-- create new neuron with calculated weight delta (previous layer (only states) also needed: leftLayer)
calcNeuronDeltaWeights :: [Neuron] -> Neuron -> [Neuron] -> Neuron
calcNeuronDeltaWeights ll rn c = updatedNeuron where
	leftLayerStates = makeStateListOfLayer ll []	
	calcedWeights = calcDeltaWeight leftLayerStates rn (deltaWeights rn) []
	updatedNeuron = setDeltaWeights rn calcedWeights

-- calc recursivly weights between n and rn
-- @param n = neuron
-- @param w/weights - weights of neuron
-- @param rn = right neuron (using ONLY delta of its => TODO?)
-- @param result = recursion result
calcDeltaWeight :: [Double] -> Neuron -> [Double] -> [Double] -> [Double]
calcDeltaWeight [] rn [] c = c
calcDeltaWeight (s:st) rn (w:dws) c = calcDeltaWeight st rn dws tmp where
	-- formla: W(D)[2][1][1] = L * (D)[3][1] * N[2][1] + M * this(t-1)
	deltaWeight = learnRate * (delta rn) * s + momentum * w
	tmp = c ++ [deltaWeight]
-- Don't use momentum (not need for first learn iteration (no deltaWeights)	
calcDeltaWeight (s:st) rn [] c = calcDeltaWeight st rn [] tmp where
	-- formla: W(D)[2][1][1] = L * (D)[3][1] * N[2][1]
	deltaWeight = learnRate * (delta rn) * s
	tmp = c ++ [deltaWeight]
	
--
-- BACKWARD PASS (step 3b)
--
-- update weights of given layer
updateLayerWeights :: [Neuron] -> [Neuron] -> [Neuron]
updateLayerWeights [] result = result
updateLayerWeights (n:layer) result = updateLayerWeights layer tmp where
	updatedWeightsOfNeuron = updateNeuronWeights n (weights n) (deltaWeights n)
	tmp = result ++ [updatedWeightsOfNeuron]

-- caclulate new weight with the formula: W[2][1][1](t+1) = W[2][1][1](t) + WD[2][1][1]
updateNeuronWeights :: Neuron -> [Double] -> [Double] -> Neuron
updateNeuronWeights neuron w deltaW = updatedNeuron where
	newWeights = zipWith (+) w deltaW
	updatedNeuron = setWeights neuron newWeights
