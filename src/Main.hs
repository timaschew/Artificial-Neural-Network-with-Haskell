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
import GraphicInterface

import Text.Printf
import Data.Char
import System.Environment

{--

use main and call there some examples for GHC compiler
GHCi can call examples directly

showError 'network'
	show delta (quadratic error) for all output neurons

showOutput 'network' 'traindata' :: [Double]
	print output for traindata with the given network

--}

main = do
	args <- getArgs

	let usecase = (args !! 0)
	let steps	| (length args >= 2) = readI (args !! 1)
			| otherwise = 100
	let saveAction	| (length args == 3) = (args !! 2)
			| otherwise = ""
			
	--putStrLn (usecase ++ " - " ++ (show steps))
	case (args !! 0) of
		"pgm1" -> universalUseCase (staticPPMexample steps) saveAction
		"xor" -> universalUseCase (xorExample steps) saveAction
	
	
test :: [Double] -> Double
test xs = sum xs

universalUseCase net action = do
	pureNet <- net
	let a = saveWeights pureNet action
	a
	showError pureNet

xorExample steps = do
	net <- initNetworkFromFile (dataPath ++ "traindata/xor/topology")
	tdata <- initTraindata (dataPath ++ "traindata/xor/trainingdata")
	let trainedNet = trainNet net tdata steps
	return trainedNet
	
staticPPMexample 0 = staticPPMexample 100
staticPPMexample steps = do
	let path = dataPath ++ "traindata/img/10_12_times/big_numbers/"
	in0 <- readPPMFile (path ++ "0_b.pgm")
	in1 <- readPPMFile (path ++ "1_b.pgm")
	in2 <- readPPMFile (path ++ "2_b.pgm")
	in3 <- readPPMFile (path ++ "3_b.pgm")
	in4 <- readPPMFile (path ++ "4_b.pgm")
	in5 <- readPPMFile (path ++ "5_b.pgm")
	in6 <- readPPMFile (path ++ "6_b.pgm")
	in7 <- readPPMFile (path ++ "7_b.pgm")
	in8 <- readPPMFile (path ++ "8_b.pgm")
	in9 <- readPPMFile (path ++ "9_b.pgm")
	
	let out0 = [1,0,0,0,0,0,0,0,0,0]
	let out1 = [0,1,0,0,0,0,0,0,0,0]
	let out2 = [0,0,1,0,0,0,0,0,0,0]
	let out3 = [0,0,0,1,0,0,0,0,0,0]
	let out4 = [0,0,0,0,1,0,0,0,0,0]
	let out5 = [0,0,0,0,0,1,0,0,0,0]
	let out6 = [0,0,0,0,0,0,1,0,0,0]
	let out7 = [0,0,0,0,0,0,0,1,0,0]
	let out8 = [0,0,0,0,0,0,0,0,1,0]
	let out9 = [0,0,0,0,0,0,0,0,0,1]
	
	let tdata = Trainingdata 10 [in0,in1,in2,in3,in4,in5,in6,in7,in8,in9] [out0,out1,out2,out3,out4,out5,out6,out7,out8,out9]
	-- input neurons 10 x 12 = 120
	net <- initNetwork "120b\n12b\n10"
	let trainedNet = trainNet net tdata steps
	return trainedNet


anotherExample = do
	putStrLn "example"