{--

artificial neural network with backpropagation as teaching method.

backpropagation algo:
1) forward pass
2) calculate error
3) backward pass


topology:
- at least 1 hidden layer
- feedforward (no shortcuts, each neuron has a connection to each neuron of the next layer, without any back connections)


TODO:
- fa(x): activate function
- fo(x): output function fo(fa(x)) 

- learnrate
- momentum
- expected teaching values
- teaching steps count
- more neuron attributes: name, delta... ????
- add more boilerplate code for (automated) testing
--}


module Main where
import Neuron

-- sigmoid function
sigmoidFunction :: (Floating a) => a -> a
sigmoidFunction x = 1 / (1 + (exp (-x)))

-- calculate state * weight + offset
-- use offset for calculation in previous recursion
calc :: [Neuron] -> [Double] -> Double -> Double
calc [] [] c = c
calc (n:ns) (x:xs) c = calc (ns) (xs) ((state n) * x + c)

-- TODO: Shoud  be tested, whether the result is correct
-- dynamic calculating from leftLayer to rightLayer
-- recursion: call calcLayer with leftLayer an rs and calcedFirstNeuron
-- calcedFirstNeuron: previousNeuronList (c) ++ current Neuron (calculated with Neuron constructor) as List
calcLayer :: [Neuron] -> [Neuron] -> [Neuron] -> [Neuron]
calcLayer leftLayer [] c = c
calcLayer leftLayer (r:rs) c = calcLayer leftLayer (rs) (c ++ (Neuron (iSum) (sta) (weight r)):[])
	where
	iSum = calc leftLayer (weight r) 0
	sta = sigmoidFunction (calc leftLayer (weight r) 0)

printState :: [Neuron] -> IO()
printState neuronList = do
	let stateList = map (\n -> state n) neuronList
	let result = zipWith (\state i ->  "n[" ++ show i ++ "].state = " ++ show state) stateList [1..]
	putStr $ unlines result

-- #################################
-- Topology and Neuron Configuration

-- input
n1_1 = Neuron 0 1.000 []
n1_2 = Neuron 0 0.000 []
n1_3 = Neuron 0 0.000 []
-- hidden
n2_1 = Neuron 0 0 [0.678, 0.211, -0.761]
n2_2 = Neuron 0 0 [0.033, -0.429, -0.938]
n2_3 = Neuron 0 0 [0.763, -0.904, -0.330]
n2_4 = Neuron 0 0 [0.223, 0.194, 0.189]
-- output
n3_1 = Neuron 0 0 [0.632, 0.952, 0.742, -0.968]

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

-- start calculating

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

main::IO()
main = do

	--putStrLn $ "In << " ++ (printState inputLayer "")
	--putStrLn $ "Out >> " ++ (printState calcedOutput "")
	
	printState $ concat calcedNetwork
	