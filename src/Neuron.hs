module Neuron where

data Neuron = Neuron {
	inputSum :: Double, -- sum of all neurons from previous layer
	state :: Double, -- f(sum) = sigmoidFunciton(sum) - fire this value to all neurons of next layer
	weight :: [Double] -- weights for the neurons of the next layer
	} deriving (Show)

type Network = [[Neuron]]
