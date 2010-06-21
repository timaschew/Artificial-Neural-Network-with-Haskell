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
import Control.Monad

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
		"num" -> universalUseCase (numberPPMexample steps) saveAction
		"na" -> universalUseCase (numAlphaPPMexample steps) saveAction
	
	
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
	
	let inputValues = [in0,in1,in2,in3,in4,in5,in6,in7,in8,in9]
	let outputValues = getOutputMatrix (length inputValues)
	
	let tdata = Trainingdata (length inputValues) inputValues outputValues
	-- input neurons 10 x 12 = 120
	net <- initNetwork "120b\n12b\n10"
	let trainedNet = trainNet net tdata steps
	return trainedNet


anotherExample = do
	putStrLn "example"


demo = do
	i1 <- readPPMFile (dataPath ++ "traindata/img/10_12_times/big_numbers/1_b.pgm")
	i1' <- readPPMFile (dataPath ++ "traindata/img/10_12_times/1.pgm")
	j1 <- readPPMFile (dataPath ++ "traindata/img/10_12_lucida/big_numbers/1_b.pgm")
	i8 <- readPPMFile (dataPath ++ "traindata/img/10_12_times/big_numbers/8_b.pgm")
	i9 <- readPPMFile (dataPath ++ "traindata/img/10_12_times/big_numbers/9_b.pgm")
	j8 <- readPPMFile (dataPath ++ "traindata/img/10_12_lucida/big_numbers/8_b.pgm")
	j9 <- readPPMFile (dataPath ++ "traindata/img/10_12_lucida/big_numbers/9_b.pgm")
	goodNet <- numberPPMexample 100
	--goodNet <- staticPPMexample 100
	--goodNet <- loadWeights "../pgm_net_weights_2000"
	
	showOutput goodNet i1
	showOutput goodNet i1'
	showOutput goodNet j1
	showOutput goodNet i8
	showOutput goodNet i9
	showOutput goodNet j8
	showOutput goodNet j9
	
demo2 = do
	ia <- readPPMFile (dataPath ++ "traindata/img/10_12_times/A.pgm")
	i1 <- readPPMFile (dataPath ++ "traindata/img/10_12_times/1.pgm")
	goodNet <- numAlphaPPMexample 200
		
	showOutput goodNet ia
	showOutput goodNet i1
	

numberPPMexample :: Int -> IO Network
numberPPMexample steps = do
	let path = dataPath ++ "traindata/img/10_12_times/big_numbers/"
	tdata <- dirToTrainData path
	net <- initNetworkFromTdata tdata
	
	return (trainNet net tdata steps)

numAlphaPPMexample :: Int -> IO Network
numAlphaPPMexample steps = do
	let path = dataPath ++ "traindata/img/10_12_times/"
	tdata <- dirToTrainData path
	net <- initNetworkFromTdata tdata
	
	return (trainNet net tdata steps)
