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

-- input
n1_1 = Neuron 0 1.000 []
n1_2 = Neuron 0 0.500 []
-- hidden
n2_1 = Neuron 0 0 [0.123, 0.234]
n2_2 = Neuron 0 0 [0.987, 0.876]
-- output
n3_1 = Neuron 0 0 [0.264, 0.633]

network = [[n1_1, n1_2], [n2_1, n2_2], [n3_1]]


calc inputNeurons outputNeuron = do
	let state1 = state (inputNeurons !! 0) 
	let state2 = state (inputNeurons !! 1)
	let inputSum1 = state1 * ((weight outputNeuron) !! 0)
	let inputSum2 = state2 * ((weight outputNeuron) !! 1)
	return (inputSum1 + inputSum2)

main::IO()
main = do

	tmp <- calc [n1_1, n1_2] n2_1
	let n2_1_n = Neuron (tmp) (sigmoidFunction tmp) (weight n2_1)
	tmp <- calc [n1_1, n1_2] n2_2
	let n2_2_n = Neuron (tmp) (sigmoidFunction tmp) (weight n2_2)
	putStrLn $ "n2_1: " ++ show n2_1_n
	putStrLn $ "n2_2: " ++ show n2_2_n
	
	tmp <- calc [n2_1_n, n2_2_n] n3_1
	let n3_1_n = Neuron (tmp) (sigmoidFunction tmp) (weight n3_1)
	putStrLn $ "n3_1: " ++ show n3_1_n
	
	putStrLn $ "In << (" ++ show (state n1_1) ++ ", " ++ show (state n1_2) ++ ")"
	putStrLn $ "Out >> " ++ show (state n3_1_n)