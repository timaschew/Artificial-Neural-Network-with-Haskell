module Neuron where

data Neuron = Neuron {
	inputSum :: Double, -- sum of all neurons from previous layer
	state :: Double, -- f(sum) = sigmoidFunciton(sum) - fire this value to all neurons of next layer
	weights :: [Double], -- weights for the neuron for the inputs of the previous layer
	delta :: Double, -- quadratic error (diff between expected and calculated value)
	deltaWeights :: [Double] -- for the main formula with learnrate and momentum
	} deriving (Show, Eq)
	
type Network = [[Neuron]]

defaultNeuron = Neuron { inputSum = 0, state = 0, weights = [], delta = 0, deltaWeights = [] } -- default Neuron
makeNeuron i s w = Neuron i s w 0 [] -- create Neuron with the three parameter (inputSum, state and weights)
setDelta n d = Neuron (inputSum n) (state n) (weights n) d (deltaWeights n) -- alternative delta setter
setDeltaWeights n dw = Neuron (inputSum n) (state n) (weights n) (delta n) dw -- alternative weight setter
setWeights n w = Neuron (inputSum n) (state n) (w) (delta n) (deltaWeights n) -- alternative weight setter
setState n s = Neuron (inputSum n) s (weights n) (delta n) (deltaWeights n)

-- sigmoid function
sigmoidFunction :: (Floating a) => a -> a
sigmoidFunction x = 1 / (1 + (exp (-x)))


