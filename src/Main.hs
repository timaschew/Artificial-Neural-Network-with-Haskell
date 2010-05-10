module Main where

import Control.Monad

type Position = (Integer, Integer) -- (Layer, Position/Index)
type NeuronStructure = [(Position, Neuron)] -- saves all neurons (topology)

data Neuron = Neuron {
	pos :: Position, -- index
	inputSum :: Double, -- sum: weights * state from previous neurons
	state :: Double -- f(sum), f = sigmoidFunction()
	} deriving (Show)
	
	
data NeuronEdge = NeuronEdge {
	from :: Position, -- (Layer, Pos)
	to :: Position, -- (Layer, Pos)
	weight :: Double -- weight between from and to
	} deriving (Show)

-- sigmoid function
sigmoidFunction :: (Floating a) => a -> a
sigmoidFunction x = 1 / (1 + (exp (-x)))

-- return Neuron for layer ans position if available
getNeuronAtPos layer position structure = do
	forM_ [1..(length structure)] $ \y -> do
		forM_ [0..(length (structure !! y))] $ \x -> do
		print "search and get neuron if available"


activateNeuron layer = do
	-- make loop over all neurons with layer <layer>
	-- calc inputSum of the neuron by NeuronEdge (weights) and Neurons
	-- set state to sigmoidFunction(inputSum)
	print ""

main::IO()
main = do

	-- input
	let n1_1 = Neuron (1,1) 0 1.000
	let n1_2 = Neuron (1,2) 0 0.500
	-- hidden
	let n2_1 = Neuron (2,1) 0 0
	let n2_2 = Neuron (2,2) 0 0
	-- output
	let n3_1 = Neuron (3,1) 0 0
	
	-- edges
	let ne211 = NeuronEdge (1,1) (2,1) 0.000
	let ne212 = NeuronEdge (1,2) (2,1) 0.000
	let ne221 = NeuronEdge (1,1) (2,2) 0.000
	let ne222 = NeuronEdge (1,2) (2,2) 0.000
	let ne311 = NeuronEdge (2,1) (3,1) 0.000
	let ne312 = NeuronEdge (2,2) (3,1) 0.000
	
	print ""