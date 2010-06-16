{--

for difficult / complex things use Issues on Github
http://github.com/timaschew/Artificial-Neural-Network-with-Haskell/issues

--}

module Main where
import Neuron
import Trainingdata
import Backpropagation
import Utils
import Config
import TopologyParser
import TraindataParser
import Menu

import Text.Printf
import Data.Char
import System.Environment

main = do
	net <- (xorExample 0)
	showError net

xorExample 0 = xorExample 2000
xorExample steps = do
	net <- initNetworkFromFile (dataPath ++ "traindata/xor/topology")
	tdata <- initTraindata (dataPath ++ "traindata/xor/trainingdata")
	let trainedNet = trainNet net tdata steps
	return trainedNet
	
	
anotherExample = do
	putStrLn "example"