{--

TODO:

for difficult / complex things use Issues on Github
http://github.com/timaschew/Artificial-Neural-Network-with-Haskell/issues

- test genericTraining function
- use foldl instead of recursivly list operations
- error handling using network with layer length <3 
- use Haddock: http://www.haskell.org/haddock/
- interface between trainingdata file/parser and function calcError
- add more boilerplate code for (automated) testing 
	- topology parser
	- network parser
	- learning parser
	
QUESTIONs:
- avoid creating new list. use network in place instead? (calcLayer)

--}

module Main where
import Neuron
import Trainingdata
import Backpropagation
import Utils
import Config
import TopologyParser
import TraindataParser

import Text.Printf
import Data.Char
import System.Environment


{--
when call main with args, then args count can be 1 or 2
first arg is the topolgy file
second arg is optional and is the traindata file

when no args used, ask the user to configure the topology typing per console

traindata can be loaded with a function which can be called from main menu

main menu = shows after every action
1 - show topology
2 - show traindata file (count teaching steps)
3 - train
4 - work
5 - exit

--}


getDefaultTopology :: String
getDefaultTopology = "../data/topology"

getDefaultTrainData :: String
getDefaultTrainData = "../data/trainingdata"	-- XOR training-file

main = do
	-- TODO add main parameter for topology and traindata file
	-- TODO: a batch file instead/alternative to onWork

	-- init traindata
	tdata <- initTraindata (dataPath ++ "trainingdata")

	-- init network
	--network <- initNetwork  "2b\n2b\n1\n"
	network <- initNetworkFromFile  "../data/topology" -- XOR training-file
	
	-- train network
	let goodNet = trainNet network tdata 10000 -- (* 4 learnsteps)
	
	-- start menu loop
	menuLoop goodNet

menuLoop goodNet = do
	printMenu	
	action <- getLine
	
	-- choose action
	let doAction | action == "1" = prettyPrint goodNet
				 | action == "2" = dummyAction
				 | action == "3" = dummyAction
				 | action == "4" = onWork goodNet
				 | action == "5" = dummyAction				 
				 | action == "0" = return()
				 | otherwise = onError
	
	-- invoke action
	doAction
	if action /= "0"
		then menuLoop goodNet
		else putStrLn "\nGoodbye!"

	
printMenu :: IO ()
printMenu = do
	printf "-----------------------\n"
	printf "--  Main Menu\n"
	printf "-----------------------\n"
	printf "[1] show topology\n"
	printf "[2] show traindata file\n"
	printf "[3] train ann\n"
	printf "[4] work\n"
	printf "[0] exit\n"
	printf "-----------------------\n"
	printf "select one action: "
	
onError :: IO ()	
onError = do
	putStrLn "\nunknown action!\n"
	--main

onWork :: Network -> IO ()
onWork net = do
	printf "\n[work] please enter some ann input (e.g: 1 0): \n"
	inputStr <- getLine
	-- TODO: check input for numbers or/and catch exception	
	let values = map (\w -> read w::Double) (words inputStr)
	
	if length values == 0
		then return()	-- leave onWork loop
		else do
			 putStrLn "please wait..."
			 let resultList = work net values
			 putStr "["
			 mapM_ (\v -> printf "%.3f " v) resultList
			 putStr "]"			 
			 --print $ work net values
			 onWork net					-- stay in onWork loop

dummyAction :: IO()
dummyAction = do
	putStrLn "\nOooops! This action is not implemented yet.\n"

-- wrapps initNetwork to generate the network from file instead from string.
initNetworkFromFile :: String -> IO Network
initNetworkFromFile filename = do
	input <- readFile filename
	initNetwork input

-- generates a network from input string
initNetwork :: String -> IO Network
initNetwork input = do
	--input <- readFile filename
	randNums <- mapM (\x -> getRandNum) [1..countNeededRandNums input]
	let net = getTopology input randNums
	; return net

	
initTraindata :: String -> IO Trainingdata
initTraindata filename = do
	input <- readFile filename
	let tdata = getTrainingdata input 4
	; return tdata

-- let train the net <steps> times
-- the function / algorithm is very slow :(
-- 5000 * 4 steps ~ 16 seconds (on a macbook 2GHz)
trainNet :: Network -> Trainingdata -> Int -> Network
trainNet net tdata 0 = {-# SCC "trainNet" #-} net
trainNet net tdata steps = trainNet trained tdata (steps-1) where
	trained = genericTraining net tdata 0
	
	
-- how to use:
-- call 'work goodNet [1,0]' for showing result for the given input
work :: Network -> TrainData -> [Double]
work net inputData = result where
	inputted = setTrainToInputLayer net inputData
	forwarded = forwardPass inputted []
	result = makeStateListOfLayer (last forwarded) []
