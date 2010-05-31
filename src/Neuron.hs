module Neuron where

data Neuron = Neuron {
	inputSum :: Double, -- sum of all neurons from previous layer
	state :: Double, -- f(sum) = sigmoidFunciton(sum) - fire this value to all neurons of next layer
	weights :: [Double], -- weights for the neuron for the inputs of the previous layer
	delta :: Double, -- quadratic error (diff between expected and calculated value)
	deltaWeights :: [Double], -- for the main formula with learnrate and momentum
	bias :: Bool
	} deriving (Show, Eq)
	
type Network = [[Neuron]]

 -- bias
biasNeuron = Neuron { inputSum = 0, state = 1, weights = [], delta = 0, deltaWeights = [], bias = True }
-- default Neuron
defaultNeuron = Neuron { inputSum = 0, state = 0, weights = [], delta = 0, deltaWeights = [], bias = False } 
-- update Neuron with the three parameter (inputSum, state, weights, bias)
updateNeuron n i s w = Neuron checkI checkS checkW (delta n) (deltaWeights n) (bias n) where
	checkI 	| (bias n) == True = 0
		| otherwise = i
	checkS 	| (bias n) == True = 1
		| otherwise = s
	checkW 	| (bias n) == True = (weights n)
		| otherwise = w
-- alternative delta setter
setDelta n d = Neuron (inputSum n) (state n) (weights n) check (deltaWeights n) (bias n) where
	check 	| (bias n) == True = 0
		| otherwise = d
-- alternative weight setter
setDeltaWeights n dw = Neuron (inputSum n) (state n) (weights n) (delta n) check (bias n) where
	check 	| (bias n) == True = (deltaWeights n)
		| otherwise = dw 
-- alternative weight setter
setWeights n w = Neuron (inputSum n) (state n) check (delta n) (deltaWeights n) (bias n) where
	check 	| (bias n) == True = (weights n)
		| otherwise = w 
-- alternative state setter
setState n s = Neuron (inputSum n) check (weights n) (delta n) (deltaWeights n) (bias n) where
	check 	| (bias n) == True = 1
		| otherwise = s

-- sigmoid function
-- 	   1
-- ____________________
--	   - (1/10) * x
-- 1 + e ^
sigmoidFunction :: (Floating a) => a -> a
sigmoidFunction x = 1 / (1 + (exp (-((1/10)*x))))


