module Main where
import Neuron
import Trainingdata
import Backpropagation
import Utils
import TopologyParser
import TraindataParser
import GraphicInterface

import Text.Printf
import Data.Char
import System.Environment
import Control.Monad

{--

use main and call there some examples for GHC compiler
GHCi can call examples directly

--}

main = do
	args <- getArgs

	let usecase = (args !! 0)
	let steps	| (length args >= 2) = readI (args !! 1)
			| otherwise = 100
	let saveAction	| (length args == 3) = (args !! 2)
			| otherwise = ""
			
	case (args !! 0) of
		"pgm1" -> universalUseCase (staticPPMexample steps) saveAction
		"xor" -> universalUseCase (xorExample steps) saveAction
		"num" -> universalUseCase (numberPPMexample steps) saveAction
		"demo" -> demo steps
		"demoXor" -> demoXor steps

test :: [Double] -> Double
test xs = sum xs

universalUseCase net action = do
	pureNet <- net
	putStrLn (show pureNet)
	showError pureNet

------------------------------------------------------------------------
-- trained networks
------------------------------------------------------------------------
xorExample steps = do
	net <- initNetworkFromFile (dataPath ++ "traindata/xor/topology")
	tdata <- initTraindata (dataPath ++ "traindata/xor/trainingdata")
	let trainedNet = trainNet net tdata steps 0.35 0.15
	return trainedNet
	
staticPPMexample 0 = staticPPMexample 100
staticPPMexample steps = do
	let path = dataPath ++ "traindata/img/17_22_lucida/"
	in0 <- readPPMFile (path ++ "0.pgm")
	in1 <- readPPMFile (path ++ "1.pgm")
	in2 <- readPPMFile (path ++ "2.pgm")
	in3 <- readPPMFile (path ++ "3.pgm")
	in4 <- readPPMFile (path ++ "4.pgm")
	in5 <- readPPMFile (path ++ "5.pgm")
	in6 <- readPPMFile (path ++ "6.pgm")
	in7 <- readPPMFile (path ++ "7.pgm")
	in8 <- readPPMFile (path ++ "8.pgm")
	in9 <- readPPMFile (path ++ "9.pgm")
	
	let inputValues = [in0,in1,in2,in3,in4,in5,in6,in7,in8,in9]
	let outputValues = getOutputMatrix (length inputValues)
	
	let tdata = Trainingdata (length inputValues) inputValues outputValues
	-- input neurons 10 x 12 = 120
	net <- initNetwork "374b\n10b\n10"
	let trainedNet = trainNet net tdata steps 0.35 0.15
	return trainedNet

numberPPMexample :: Int -> IO Network
numberPPMexample steps = do
	let path = dataPath ++ "traindata/img/10_12_arial/"
	tdata <- dirToTrainData path
	net <- initNetworkFromTdata tdata
	
	return (trainNet net tdata steps 0.35 0.15)	

------------------------------------------------------------------------
-- DEMOS
------------------------------------------------------------------------
demo steps = do
	let path = "traindata/img/10_12_arial/"
	tdata <- dirToTrainData (dataPath ++ path)
	nt <- initNetworkFromTdata tdata
	let net = trainNet nt tdata steps 0.35 0.15
	
	putStrLn ("training folder: " ++ path)
	showNumsOutput net path
	putStrLn "----------------------------------"
	putStrLn "pattern folder: traindata/img/10_12_great_times/"
	showNumsOutput net "traindata/img/10_12_great_times/"
	
demoXor steps = do
	trainedNet <- xorExample steps
	putStr "0.0  XOR  0.0 = "
	showOutput trainedNet [0.0, 0.0]
	putStr "1.0  XOR  0.0 = "
	showOutput trainedNet [1.0, 0.0]
	putStr "0.0  XOR  1.0 = "
	showOutput trainedNet [0.0, 1.0]
	putStr "1.0  XOR  1.0 = "
	showOutput trainedNet [1.0, 1.0]
