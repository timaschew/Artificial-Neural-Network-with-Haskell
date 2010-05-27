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
main::IO()
main = do
	
	--printState $ concat forwardedNetwork
	--putStr $ "output: "
	--printState $ concat [last forwardedNetwork]
	print goodNet
	
-- #################################
-- Topology and Neuron Configuration
-- input
b1_0 = biasNeuron 
n1_1 = defaultNeuron
n1_2 = defaultNeuron
-- hidden
b2_0 = biasNeuron { weights = [0,0,0] }
n2_1 = defaultNeuron { weights = [0.033, -0.429, -0.938] }
n2_2 = defaultNeuron { weights = [0.763, -0.904, -0.330] }
-- output
n3_1 = defaultNeuron { weights = [0.632, 0.952, 0.742] }

network = [[b1_0, n1_1, n1_2], [b2_0, n2_1, n2_2], [n3_1]]
-- #################################

--
-- Start generic way
--

-- XOR trainingdata
inputValues = [[0,0],[0,1],[1,0],[1,1]]
outputValues = [[0],[1],[1],[0]]
tdata = Trainingdata 4 inputValues outputValues

-- HERE IT IS - The result from the generic backpropagation algorithm (only 4 learnsteps)
trainedNet = genericTraining network tdata 0

goodNet = trainNet network 10000 -- (* 4 learnsteps)

-- set input layer with the given TrainData and call forwardPass
-- show only state of the neuron(s) of output layer
-- exmaple use case:
-- 1 start GHCi
-- 2 call 'goodNet' and wait - calculates the network
-- 3 call 'demo goodNet [1,0]' for showing result for the given input
demo :: Network -> TrainData -> [Double]
demo net inputData = result where
	inputted = setTrainToInputLayer net inputData
	forwarded = forwardPass inputted []
	result = makeStateListOfLayer (last forwarded) []

-- let train the net <steps> times
-- the function / algorithm is very slow :(
-- 5000 * 4 steps ~ 16 seconds (on a macbook 2GHz)
trainNet :: Network -> Int -> Network
trainNet net 0 = {-# SCC "trainNet" #-} net
trainNet net steps = trainNet trained (steps-1) where
	trained = genericTraining net tdata 0
	
	
