module Neuron where

data Neuron = Neuron {
	inputSum :: Double, -- sum of all neurons from previous layer
	state :: Double, -- f(sum) = sigmoidFunciton(sum) - fire this value to all neurons of next layer
	weight :: [Double], -- weights for the neuron for the inputs of the previous layer
	delta :: Double, -- quadratic error (diff between expected and calculated value)
	deltaWeight :: [Double] -- for the main formula with learnrate and momentum
	} deriving (Show, Eq)

defaultNeuron = Neuron { inputSum = 0, state = 0, weight = [], delta = 0, deltaWeight = [] } -- default Neuron

makeNeuron iSum sta wei = Neuron iSum sta wei 0 [] -- old constructor compatibility
makeDelta n d = Neuron (inputSum n) (state n) (weight n) d (deltaWeight n) -- alternative delta setter

makeDeltaWeights n dw = Neuron (inputSum n) (state n) (weight n) (delta n) dw -- alternative weight setter

setWeights n w = Neuron (inputSum n) (state n) (w) (delta n) (deltaWeight n) -- alternative weight setter

type Network = [[Neuron]]
