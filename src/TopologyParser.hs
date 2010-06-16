module TopologyParser where
import Neuron
import Config
import Data.Char
import Data.List
import Data.List.Split
import Random


-- returns a random number in range (-1.0 to 1.0)
getRandNum :: IO Double
getRandNum = do randomRIO (-1, 1 :: Double)

-- calces and returns the number of needed random weights
-- each neuron has a weight list needed for calcing the input: weight * output of the previous layer neurons
-- total = prev layer size (with bias) * layer size (without bias)
countNeededRandNums :: String -> Int
countNeededRandNums input = total where
	tupleList = parseTopology (lines input)
	
	-- calc the number of inputs of the previous layer. input layer 0 inputs, output layer does not care
	prevInputs = [0] ++ map (\(n, isBias) -> if isBias then n+1 else n ) (init tupleList)
	
	-- neuron count of each layer, bias are ignored, because they dont have any inputs.
	layerNeuronCount = map (\(n, isBias) -> n ) tupleList
	
	--result = zipWith (\pn cn-> pn*cn) prevInputs layerNeuronCount
	total = sum $ zipWith (*) prevInputs layerNeuronCount


-- returns an list of list of list: layerList -> neuronList -> neuronWeightList ==> all Network weights
makeWeightLists :: [Double] -> [(Int, Bool)] -> [Int] -> [[[Double]]]
makeWeightLists randNums [] _ = []
makeWeightLists randNums (l:layers) (p:prevInputs) = [w] ++ makeWeightLists randNums' layers prevInputs where
	-- take one layers weights from randNums
	weightList = take (p*(fst l)) randNums
	
	-- split randNums into weight sublists. every neuron of one layer has the same num of weights.
	w = splitEvery p weightList
	
	-- drop taken weights	
	randNums' = drop (p*(fst l)) randNums


-- input: whole file input str
-- randNums: a list of random Double values, the list size is equal to the total needed neuron weights in the network.
-- returns the generated Network
getTopology :: String -> [Double] -> Network
getTopology input randNums = result where
	tupleList = parseTopology (lines input)
	
	-- calc the number of inputs of the previous layer. input layer has 0 inputs, output layer does not care
	prevInputs = [0] ++ map (\(n, isBias) -> if isBias then n+1 else n ) (init tupleList)
	
	-- fill input layer with empty lists, because input neurons dont have weights
	firstLayerInputs = [map (\x -> []) [1.. (fst $ head tupleList)]]
	
	-- skip first tuple and prevInputs elements (no random weights needed)
	weightLists = firstLayerInputs ++ makeWeightLists randNums (tail tupleList) (tail prevInputs)
	
	-- generate network. prevInputs, tupleList and weightLists must have the same length == layer count
	result = zipWith3 (\p (n, b) w -> generateLayer p n b w) prevInputs tupleList weightLists


-- generates a list of Neurons (Layer) with random weights.
generateLayer :: Int -> Int -> Bool ->  [[Double]] -> [Neuron]
generateLayer prevN curN isBias randNums = result where
	-- randNums is a Double list of list, which represents the random weights of the current layer neurons
	neuronList = map (\x -> defaultNeuron {weights = x}) randNums
	
	-- return the neuronList and add an biasNeuron if needed
	result | isBias && curN > 0 = [biasNeuron {weights = (map (\x -> 0.0) [1..prevN])}] ++ neuronList
	       | otherwise = neuronList


-- build a list of tuples. A tuple describes a Layer. 
-- Int stands for the num of Neurons and the Bool for containing a Bias Neuron.
parseTopology :: [String] -> [(Int, Bool)]
parseTopology [] = []
parseTopology (l:lines) = tup ++ (parseTopology lines) where
	hasData = isDataLine l
	lineData = if hasData
					then getLineData l	
					else (0, False)
	tup | hasData = [(fst lineData, snd lineData)]
		| otherwise = []	-- nothing to add

	
-- takes the first value in line. Other following whitespace separated values will be ignored
-- accepted formats for 2 neurons: 2, 2b, b2
-- splits the line into Int and String part: => ("2", "") or ("2", "b"), ("b", "2"), ... and returns a (Int, Bool) tuple
getLineData :: String -> (Int, Bool)
getLineData line = result where
	val | length (words line) > 0 = head (words line)
		| otherwise = "0"

	biasIdx = 'b' `elemIndices` val -- find bias position

	tup | length biasIdx > 0  &&  head biasIdx > 0 = splitAt (head biasIdx) val
		| length biasIdx > 0  &&  head biasIdx == 0 = splitAt 1 val		-- fix problem with leading 'b' like "b2" => ("", "2b")
		| otherwise = splitAt (length val) val
		
	neuronCount | length (fst tup) > 0  &&  all isNumber (fst tup) = read (fst tup) ::Int
				| length (snd tup) > 0  &&  all isNumber (snd tup) = read (snd tup) ::Int
				| otherwise = 0
				
	isBias | length (fst tup) > 0  &&  (fst tup) == "b" = True
		   | length (snd tup) > 0  &&  (snd tup) == "b" = True
		   | otherwise = False

	result = (neuronCount, isBias)


-- returns False if the line starts with an '#' or '--' or is empty
isDataLine :: String -> Bool
isDataLine line = result where
	tmp = dropWhile isSpace line -- eliminate leading spaces
	result | (length tmp == 0) = False
		   | (length tmp > 1) && ("#" `isPrefixOf` tmp) = False
		   | (length tmp > 2) && ("--" `isPrefixOf` tmp) = False
		   | otherwise = True
