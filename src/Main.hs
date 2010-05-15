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
calc :: [Neuron] -> [Double] -> Double -> Double
calc [] [] c = c
calc (n:ns) (x:xs) c = calc (ns) (xs) ((state n) * x + c)

-- TODO: Shoud  be tested, whether the result is correct
-- dynamic calculating from leftLayer to rightLayer
-- works ONLY with 3 layers (1x input, 1x hidden, 1x output)
-- recursion: call calcLayer with leftLayer an rs and calcedFirstNeuron
-- calcedFirstNeuron: previousNeuronList (c) ++ current Neuron (calculated with Neuron constructor) as List
calcLayer :: [Neuron] -> [Neuron] -> [Neuron] -> [Neuron]
calcLayer leftLayer [] c = c
calcLayer leftLayer (r:rs) c = calcLayer leftLayer (rs) (c ++ (Neuron (iSum) (sta) (weight r)):[])
	where
	iSum = calc leftLayer (weight r) 0
	sta = sigmoidFunction (calc leftLayer (weight r) 0)

-- TODO: remove this style of output: "\"\\\"\\\" 1.0\" 0.0" 0.0
-- 	 let the output look like this: 1.0 0.0 0.0
-- print state of [Neuron]
printState :: [Neuron] -> [Char] -> [Char]
printState (n:[]) c = (show c ++ " " ++ (show (state n)))
printState (n:list) c = printState list (show c ++ " " ++ (show $(state n)))
-- wrong order but wihout escaped backslashes: (show $ state n) ++ ", " ++ c

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

-- works ONLY with 3 layers (1x input, 1x hidden, 1x output)
inputLayer = network !! 0
hiddenLayer = network !! 1
outputLayer = network !! 2

calcedHiddenLayer :: [Neuron]
calcedHiddenLayer = calcLayer inputLayer hiddenLayer []

calcedOutput :: [Neuron]
calcedOutput = calcLayer calcedHiddenLayer outputLayer []

main::IO()
main = do

	putStrLn $ "In << " ++ (printState inputLayer "")
	putStrLn $ "Out >> " ++ (printState calcedOutput "")
	