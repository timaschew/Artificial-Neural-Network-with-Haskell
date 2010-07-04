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
l = layer (list of neurons)
ll = left layer
rl = right layer
n = neuron (or rn = neuron of right layer, ln = neuron of left layer)
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
3c	  calcLayerDelta = calculate delta / error for layer X (like step 2, but only for hidden layer)
	  	{ calcNeuronDelta = calculate delta / error for one neuron of layer X }}
	 }
	 
genericTraining = do all this steps (1-3c)

setTrainToInputLayer = set state of input layer neurons, for training or working phase

--}

module Backpropagation where
import Neuron
import Trainingdata
import Data.List (foldl')
import Control.Parallel.Strategies	-- cabal install parallel


-- Backpropagation Configuration
learnRate = 0.350
momentum = 0.15

--
-- GENERIC Algorithm,
--
-- do forwardPass with TrainData then backwardPass with TrainData
-- call genericTraining network trainingdata 0
-- important to use zero for last parameter
genericTraining :: Network -> Trainingdata -> Int -> Network
genericTraining net t (-1) = net -- stop here
genericTraining net t i = genericTraining trainedNet t loop where
	inputTraining = (inputs t) !! i
	outputTraining = (outputs t) !! i
	
	readyNet = setTrainToInputLayer net inputTraining
	forwarded = forwardPass readyNet []
	trainedNet = backwardPass forwarded outputTraining
	--trainedNet = forwarded
	loop	| i == ((learnSteps t)-1) = (-1)
		| otherwise = (i+1)
		
-- update the states of the neuron in the input layer from TrainData values
setTrainToInputLayer :: Network -> TrainData -> Network
setTrainToInputLayer net train = updatedNet where
	inputLayer = head net -- get intput layer
	 -- skip first neuron, becaus its a bias
	inputNeurons	| ((bias (head inputLayer)) == True) = tail inputLayer
			| otherwise = inputLayer -- no bias use whole layer
	updatedInputLayer = setTrainToNeuron inputNeurons train
	readyLayer | ((bias (head inputLayer)) == True) = [head inputLayer] ++ updatedInputLayer
			| otherwise = updatedInputLayer
	-- use old net but update readyLayer (drop and append updated)
	updatedNet = [readyLayer] ++ (drop 1 net)

-- update the state of one neuron recursivly
setTrainToNeuron :: [Neuron] -> TrainData -> [Neuron]
setTrainToNeuron [] [] = [] 
setTrainToNeuron layer [] = setEmptyNeurons layer
setTrainToNeuron (n:layer) (t:train) = updatedNeuron : setTrainToNeuron layer train where
	updatedNeuron = setState n t
	
setEmptyNeurons :: [Neuron] -> [Neuron]
--setEmptyNeurons [] = []
setEmptyNeurons layer = foldr (\n tmp -> (setState n 0) : tmp) [] layer
--setEmptyNeurons (n:layer) = updated : setEmptyNeurons layer where
--	updated = setState n 0

-- use this method to do the 1. algo step. Example: printNet forwardPass network [[]] 
-- @params: currentNet newNet
-- @return new network without the input layer (input layer dont have to be calculated)
forwardPass :: Network -> Network -> Network
forwardPass [last] c = c ++ [last]
forwardPass (l:net) c =  forwardPass updatedLs tmp where 
	nextLayer = head net
	updatedNextLayer = calcLayer l nextLayer
	updatedLs = [updatedNextLayer] ++ (drop 1 net)
	tmp = c ++ [l]

-- preparing stuff for backPassSteps
-- calculate error (delta of output layer)
-- reverse net and call backPassSteps
backwardPass :: Network -> TrainData -> Network
backwardPass net train = trainedNet where
	outputlayer = last net
	-- step 2
	calcedErrorLayer = calcLayerError outputlayer train -- calc output error
	updatedNet = (init net) ++ [calcedErrorLayer] -- updating output layer
	reversedNet = reverse updatedNet --
	reversedTrainedNet = backPassSteps reversedNet
	trainedNet = reverse reversedTrainedNet

-- do steps 3a, 3b, 3c, network is used reversed
-- stop at input layer, no calculation needed to this layer
backPassSteps :: Network -> Network
backPassSteps [lastLayer] = [lastLayer] -- this is the input layer
backPassSteps (l:net) = newest_l : backPassSteps updatedNet where 
	-- in first recursion:
	-- first element (l) is the output layer (real last layer)
	-- because net was reverse in backwardPass
	-- and nextLayer is the layer before the the output layer
	nextLayer = (head net)
	new_l = calcLayerDeltaWeigts nextLayer l 		-- step 3a
	newest_l = updateLayerWeights new_l 			-- step 3b
	new_nextLayer = calcLayerDelta nextLayer newest_l 0	-- step 3c

	updatedNet = [new_nextLayer] ++ (drop 1 net)
	
--
-- FORWARD PASS (step 1)
--
-- dynamic calculating from leftLayer to rightLayer
-- recursion: call calcLayer with leftLayer an rs and calcedFirstNeuron
-- calcedFirstNeuron: previousNeuronList (c) ++ current Neuron (calculated with Neuron constructor) as List
calcLayer :: [Neuron] -> [Neuron] -> [Neuron]
calcLayer ll [] = []
calcLayer ll (n:rl) = updatedNeuron : calcLayer ll (rl) where 
	v_inputSum = calcNeuron ll (weights n)
	v_state = sigmoidFunction v_inputSum
	updatedNeuron = updateNeuron n v_inputSum v_state (weights n)

-- calculate state * weight + offset
-- use offset for calculation in previous recursion
-- @param [Neuron]:	Neuron list of a single layer
-- @param [Double]: Neuron weights (w.i) of layer n for a certain Neuron of layer n+1: 
--
--	[List of Neurons] ---- w.i ---->  [single Neuron] (layer n + 1)
--
-- @return Double: offset
-- @return Double: netinput (sum) for a certain Neuron of layer n + 1
calcNeuron :: [Neuron] -> [Double] -> Double
calcNeuron [] [] = 0
calcNeuron states [] = 0
calcNeuron (s:states) (w:weights) = v_inputSum + calcNeuron states weights where
	v_inputSum = (state s) * w

--
-- CALCULATE ERROR (step 2)
--
-- calculate error (delta) from output layer
calcLayerError :: [Neuron] -> [Double] -> [Neuron]
calcLayerError [] [] = []
calcLayerError (n:layer) (e:el) = updatedNeuron : calcLayerError layer el where
	updatedNeuron = calcError n e

-- calc error (delta) for one neuron of the output layer
calcError :: Neuron -> Double -> Neuron
calcError n expected = setDelta (n) delta where
	s = (state n)
	delta = (expected - s) * s * (1 - s)

--
-- BACKWARD PASS (step 3a)
--
-- helper function for step 3a
-- input: list of neurons (list); output: list of state values from the neurons
makeStateListOfLayer :: [Neuron] -> [Double]
-- ATTENTION! foldl returns a reversed list because of state n : tmp
--makeStateListOfLayer layer = reverse $ foldl' (\tmp n -> state n : tmp) [] layer
--makeStateListOfLayer layer = foldl' (\tmp n -> tmp ++ [state n]) [] layer -- 3x longer...
makeStateListOfLayer layer = foldr (\n tmp -> state n : tmp) [] layer

-- creates and return the rightLayer with calculated weight deltas
calcLayerDeltaWeigts :: [Neuron] -> [Neuron] -> [Neuron]
calcLayerDeltaWeigts ll [] = []
calcLayerDeltaWeigts ll (n:rl) = updatedNeuron : calcLayerDeltaWeigts ll rl where
	updatedNeuron = calcNeuronDeltaWeights ll n

-- create new neuron with calculated weight delta (previous layer (only states) also needed: leftLayer)
calcNeuronDeltaWeights :: [Neuron] -> Neuron -> Neuron
calcNeuronDeltaWeights ll rn = updatedNeuron where
	leftLayerStates = makeStateListOfLayer ll
		
	calcedWeights = calcDeltaWeight rn leftLayerStates (deltaWeights rn)
	updatedNeuron = setDeltaWeights rn calcedWeights

-- calc recursivly weights between n and rn
-- @param rn = right neuron (using ONLY delta of its => TODO?)
-- @param s = leftLayerStates
-- @param w/weights - weights of neuron
-- @param result = recursion result
calcDeltaWeight :: Neuron -> [Double] -> [Double] -> [Double]
calcDeltaWeight rn [] [] = []
calcDeltaWeight rn (s:st) (w:dws) = deltaWeight : calcDeltaWeight rn st dws where
--calcDeltaWeight rn allS@(s:st) allW@(w:dws) = foldr (\x tmp -> (calcDW rn x):tmp) [] list where
--calcDeltaWeight rn allS@(s:st) allW@(w:dws) = result where
	-- formla: W(D)[2][1][1] = L * (D)[3][1] * N[2][1] + M * this(t-1)
	deltaWeight = learnRate * (delta rn) * s + momentum * w
	--list = zip allS allW
	--result = foldr (\x tmp -> (calcDW rn x):tmp) [] list

-- Don't use momentum (not need for first learn iteration (no deltaWeights)	
calcDeltaWeight rn (s:st) [] = deltaWeight : calcDeltaWeight rn st [] where
--calcDeltaWeight rn allS@(s:st) [] = foldr (\x tmp -> (calcDW rn x):tmp) [] list where
	-- formla: W(D)[2][1][1] = L * (D)[3][1] * N[2][1]
	deltaWeight = learnRate * (delta rn) * s
--	list = map (\s -> (s, 0.0)) allS

calcDW rn x = learnRate * (delta rn) * s + momentum * w where
	s = fst x
	w = snd x

	
--
-- BACKWARD PASS (step 3b)
--
-- update weights of given layer
updateLayerWeights :: [Neuron] -> [Neuron]
updateLayerWeights [] = []
updateLayerWeights (n:layer) = updatedWeightsOfNeuron : updateLayerWeights layer where
	updatedWeightsOfNeuron = updateNeuronWeights n (weights n) (deltaWeights n)

-- caclulate new weight with the formula: W[2][1][1](t+1) = W[2][1][1](t) + WD[2][1][1]
updateNeuronWeights :: Neuron -> [Double] -> [Double] -> Neuron
updateNeuronWeights neuron w deltaW = updatedNeuron where
	newWeights = zipWith (+) w deltaW
	updatedNeuron = setWeights neuron newWeights
	
	
--
-- BACKWARD PASS (step 3c)
--
-- sum up the delta * weight_i of a neuron
getDeltaStateSumForWeight :: Int -> [Neuron] -> Double
getDeltaStateSumForWeight i [] = 0
getDeltaStateSumForWeight i (n:rl) = deltaStateOfNeuron + getDeltaStateSumForWeight i rl where
	deltaStateOfNeuron = (delta n) * ((weights n) !! i)

-- calculate the delta for a layer (not for output layer)
calcLayerDelta :: [Neuron] -> [Neuron] -> Int -> [Neuron]
calcLayerDelta [] rl i = []
calcLayerDelta (n:ll) rl i = updatedNeuron : calcLayerDelta ll rl (i+1) where
	updatedNeuron = calcNeuronDelta n rl i

-- calcuate the delta for a neuron of a layer (not from the output layer)
calcNeuronDelta	:: Neuron -> [Neuron] -> Int -> Neuron
calcNeuronDelta ln rl i = updatedNeuron where
	deltaStateSumOfNextLayer = getDeltaStateSumForWeight i rl
	formula = (state ln) * (1 - (state ln)) * deltaStateSumOfNextLayer
	updatedNeuron = setDelta ln formula
