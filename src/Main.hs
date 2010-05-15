-- Implementation of file operations and a trivial calculation functions for
-- a very simple(!) artificial neuron.
--
-- TODO
-- * connect multiple neurons. either extend Neuron type or create an
-- additional type NeuronNetwork.
-- * add trivial learning algorithm, e.g. for perceptrons.
-- * add more boilerplate code for (automated) testing
-- * if time and motivation, add backpropagation

module Main where
import Neuron

-- sigmoid function
sigmoidFunction :: (Floating a) => a -> a
sigmoidFunction x = 1 / (1 + (exp (-x)))

-- calculate state * weight + offset
-- use offset for calculation in previous recursion
-- [Neuron] => [Double] => Double
calc (n:[]) (x:[]) c = (state n) * x + c
calc (n:ns) (x:xs) c = calc (ns) (xs) ((state n) * x + c)

-- TODO: Shoud  be tested, whether the result is correct
-- dynamic calculating from leftLayer to rightLayer
-- works ONLY with 3 layers (1x input, 1x hidden, 1x output)
-- parameter: leftLayer firstNeuronOfRightLayer previousCalculatedNeurons
calcLayer :: (Monad m) => [Neuron] -> [Neuron] -> [Neuron] -> m [Neuron]
-- return: previousNeuronList (c) ++ calc last Neuron
calcLayer leftLayer (rn:[]) c = return (c ++ (Neuron (iSum) (sta) (weight rn)):[])
	where
	iSum = calc leftLayer (weight rn) 0
	sta = sigmoidFunction (calc leftLayer (weight rn) 0)
-- recursion: call calcLayer with leftLayer an rs and calcedFirstNeuron
-- calcedFirstNeuron: previousNeuronList (c) ++ current Neuron (calculated with Neuron constructor) as List
calcLayer leftLayer (r:rs) c = calcLayer leftLayer (rs) (c ++ (Neuron (iSum) (sta) (weight r)):[])
	where
	iSum = calc leftLayer (weight r) 0
	sta = sigmoidFunction (calc leftLayer (weight r) 0)

-- #################################
-- Topology and Neuron Configuration

-- input
n1_1 = Neuron 0 1.000 []
n1_2 = Neuron 0 0.500 []
n1_3 = Neuron 0 0.200 []
-- hidden
n2_1 = Neuron 0 0 [0.123, 0.234, 0.532]
n2_2 = Neuron 0 0 [0.987, 0.876, 0.642]
n2_3 = Neuron 0 0 [0.157, 0.323, 0.149]
n2_4 = Neuron 0 0 [0.634, 0.256, 0.342]
-- output
n3_1 = Neuron 0 0 [0.264, 0.633, 0.831, 0.253]

network = [[n1_1, n1_2, n1_3], [n2_1, n2_2, n2_3, n2_4], [n3_1]]
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

-- works ONLY with 3 layers (1x input, 1x hidden, 1x output)
inputLayer = network !! 0
hiddenLayer = network !! 1
outputLayer = network !! 2

-- Why its [[Neuron]] and not [Neuron] ?
calcedHiddenLayer :: [[Neuron]]
calcedHiddenLayer = calcLayer inputLayer hiddenLayer []

calcedOutput :: [[Neuron]]
calcedOutput = calcLayer (calcedHiddenLayer !! 0) outputLayer []

main::IO()
main = do

	-- TODO: function, which prints only state of Neurons
	putStrLn $ "In << (" ++ (show inputLayer) ++ ")"
	putStrLn $ "Out >> " ++ (show calcedOutput)
	putStrLn ""
	