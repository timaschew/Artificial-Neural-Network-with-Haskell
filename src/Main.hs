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


-- 5x7 Letters [A..Z]
alpha = do
	az <- readFile (dataPath ++ "traindata/img/raw/alphabet")
	let list = map (\l -> words l) (lines az)
	
	-- inputvalues: list of letter lists
	let az = splitAlphas list [] []

	-- TODO: finish test

	print az
	
	

-- split all letter lines containing list by the comment line 
-- a letter is described by a list of lines
splitAlphas :: [[String]] -> [[Int]] -> [[[Int]]] -> [[[Int]]]
splitAlphas [] [] res = res
splitAlphas [] (_:_) res = res
splitAlphas (l:ll) letter res = splitAlphas ll letter' res' where
	newLetter | length l > 0 && head (head l) == '-' = True
		      | otherwise = False

	lastElem | length ll == 0 = True
			 | otherwise = False

	-- letter finished?
	res' | (newLetter || lastElem) && length letter > 0 = res ++ [letter]
		 | otherwise = res

	lInt = map (\s -> read s ::Int ) l 

	-- line belongs to current letter?
	letter' | newLetter || lastElem = []
			| length l == 0 = letter		-- skip empty line
			| otherwise = letter ++ [lInt]



point = do
	let a1 = [1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0]
	let a2 = [0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0]
	let a3 = [0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0]
	let a4 = [0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0]
	let a5 = [0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0]
	let a6 = [0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0]
	let a7 = [0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0]
	let a8 = [0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1,0,0,0,0,0]
	let a9 = [0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,0,0,1,1,1]

	let t1 = [0,0,1,0,0,0,1,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0]
	let t2 = [0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,1,1,1,1,0,0,0,0,0]
	let t3 = [0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,1,1,1,1]
	
	let q = [1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1]
	let c = [0,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0]

	let input = [a1, a2, a5, a7, a9, q, c]
	let output = [[0,0,1], [0,0,1], [0,0,1], [0,0,1], [0,0,1], [0,1,0], [1,0,0]]
	let tdata = Trainingdata (length input) input output
	
	network <- initNetwork "b25\nb15\n3"
	
	let goodNet = trainNet network tdata 1000

	print $ work goodNet a1
	print $ work goodNet a2
	print $ work goodNet a3
	print $ work goodNet a4
	print $ work goodNet a5
	print $ work goodNet a6
	print $ work goodNet a7
	print $ work goodNet a8
	print $ work goodNet a9
	
	print $ work goodNet t1
	print $ work goodNet t2
	print $ work goodNet t3
	
	print $ work goodNet q
	print $ work goodNet c

{--
	[1.356038682134825e-3,4.33979363859194e-3,0.9965318464789291]
	[2.6958612335808508e-3,7.769883551543551e-4,0.9980546135391705]
	[9.94626556819184e-3,9.742843199743946e-2,0.6791146606006317]
	[1.137016193880069e-3,1.1157599230883202e-3,0.9987387815311596]
	[5.633237383077041e-3,3.376150088200687e-4,0.9983242964142387]
	[6.872867783913206e-3,2.5846581297256496e-3,0.981512734312351]
	[1.729765883338632e-3,4.8808807326840105e-3,0.9946241591641551]
	[1.5519524881880287e-3,1.0195675649264098e-3,0.9976475868707727]
	[2.409126535611211e-3,4.252129943468505e-3,0.9920286944661496]
	
	[2.1907002500469903e-2,4.130124610061044e-3,0.9842330130079537]
	[3.7285143024701466e-3,6.087038068783476e-4,0.9955969446219342]
	[4.5993268460037866e-2,5.2464240106995075e-2,0.14782523643847942]
	
	[8.10388046875866e-3,0.9905089612994619,8.819751218154226e-3]
	[0.9901837616474689,7.278291039980571e-3,7.16053482202491e-3]
--}


matrix = do
	let q = [1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1]
	let c = [0,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0]
	let t = [0,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,1,0,1,1,1,1,1]
	let input = [q, c, t]
	let output = [[0,0,1], [0,1,0], [1,0,0]]
	let tdata = Trainingdata 3 input output
	
	network <- initNetwork "b25\n15\n3"
	
	let goodNet = trainNet network tdata 1000
	
	print $ work goodNet [1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1]
	print $ work goodNet [0,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0]
	print $ work goodNet [0,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,1,0,1,1,1,1,1]
	
	print $ work goodNet [1,1,0,1,0,1,0,0,0,0,1,0,0,0,1,1,0,0,0,1,1,1,0,1,1]
	print $ work goodNet [1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1]
	print $ work goodNet [1,1,1,1,1,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,1,1,1,1]
	print $ work goodNet [1,1,0,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,0]
	
{-- RESULT
[1.9635942087467995e-2,1.993677533711247e-2,0.9775369193784258]		0 0 1
[1.5701105201431347e-2,0.9776522989421892,1.59188731510604e-2]		0 1 0
[0.9785254329075819,1.8888033507159722e-2,1.6846267328739853e-2]	1 0 0

[2.5301098930085156e-2,1.9586530424234132e-2,0.9547334068136564]	0 0 1
[1.9635942087467995e-2,1.993677533711247e-2,0.9775369193784258]		0 0 1
[3.5864048635665516e-2,1.4388356711030195e-2,0.9595219865693639]	0 0 1
[1.1459838908511747e-2,3.029882884195452e-2,0.9701584783409926]		0 0 1

--}
	
	
triangle = do
	let t1 = [0,0,1,0,0,0,1,0,1,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0]
	let t2 = [0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,1,1,1,1,0,0,0,0,0]
	let t3 = [0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,1,1,1,1]
	let q = [1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1]
	let c = [0,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,0,1,1,1,0]

	let input = [t1, t2, q, c]
	let output = [[0,0,1], [0,0,1], [0,1,0], [1,0,0]]
	let tdata = Trainingdata 4 input output
	
	network <- initNetwork "b25\n10\n3"
	
	let goodNet = trainNet network tdata 10000
	
	print $ work goodNet t1
	print $ work goodNet t2
	print $ work goodNet t3
	print $ work goodNet q
	print $ work goodNet c

main = do
	-- TODO add main parameter for topology and traindata file
	-- TODO: a batch file instead/alternative to onWork

	-- init traindata
	tdata <- initTraindata (dataPath ++ "trainingdata")

	-- init network
	--network <- initNetwork  "2b\n2b\n1\n"
	network <- initNetworkFromFile (dataPath ++ "topology") -- XOR training-file
	
	-- train network
	let goodNet = trainNet network tdata 1000 -- (* 4 learnsteps)
	
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
