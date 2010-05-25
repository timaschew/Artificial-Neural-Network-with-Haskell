{--

TODO:  clean up the print functions

--}

module Utils where
import Neuron
import Control.Monad
import Text.Printf

-- print states of a layer
printState :: [Neuron] -> IO()
printState neuronList = do
	let stateList = map (\n -> state n) neuronList
	let result = zipWith (\state i ->  "n[" ++ show i ++ "].state = " ++ show state) stateList [1..]
	putStr $ unlines result

-- print network (neuron index and state)
printNet :: Network -> IO()
printNet network = do
	let neuronList = concat network
	let result = zipWith (\neuron i ->  "n[" ++ show i ++ "] : " ++ show neuron) neuronList [1..]
	putStr $ unlines result


-- alternative pretty print
-- print list of strings (one element for each line)
printPretty net = splitAndNewline where
	result = prettyP net 1 []
	splitAndNewline = putStrLn (unlines result)

-- print neuron indices and state as string list
prettyP :: Network -> Int -> [String] -> [String]
prettyP [] row c = c
prettyP (l:net) row c = prettyP net (row+1) tmp where
	neurons = printNeuronLayers l row 1 []
	states = printStateLayers l ""
	tmp = c ++ [neurons] ++ [states] ++ [""]

-- print indices for a layer
printNeuronLayers :: [Neuron] -> Int -> Int -> String -> String
printNeuronLayers [] row col c = c
printNeuronLayers (n:l) row col c =  printNeuronLayers l row (col+1) tmp where
	updated = printNeuron n row col
	tmp = c ++ updated
-- print neuron index
printNeuron :: Neuron -> Int -> Int -> String
printNeuron n row col = printed  where
	i	| (bias n) == True = 0
		| otherwise = col
	printed1l = printf "N[%d][%d]     " (row::Int) (i)
	printed = printed1l

-- print state for a layer
printStateLayers :: [Neuron] -> String -> String
printStateLayers []  c = c
printStateLayers (n:l) c =  printStateLayers l tmp where
	updated = printNeuronState n
	tmp = c ++ updated
	
-- print neuron state	
printNeuronState :: Neuron -> String
printNeuronState n = printed  where
	printed1l = printf "%f         " (state n)
	printed = printed1l
	
-- alternative pretty print END


--
-- pretty print method
--
prettyPrint :: Network -> IO()
prettyPrint network = do
	mapM_ putStr (buildPrettyStr network)


buildPrettyStr :: Network -> [String]
buildPrettyStr net = result
	where
	list = buildLayerStr 1 net []
	max = getMaxLayerSize list 0
	result = concateAllNeuron max list []


-- returns: 
buildLayerStr :: Int -> Network -> [[String]] -> [[String]]
buildLayerStr curIndex [] res = res
buildLayerStr curIndex (l:layerList) res = buildLayerStr nextIndex layerList tmp
	where
	nextIndex = curIndex + 1
	-- check if first neuron is a bias or not
	realNeuronIndex	| (bias (head l)) == True = 0
			| otherwise = 1
	tmp = res ++ (buildNeuronStr curIndex realNeuronIndex l [] [])


buildNeuronStr :: Int -> Int -> [Neuron] -> [String] -> [String] -> [[String]]
buildNeuronStr i j [] r1 r2 = [r1] ++ [r2]
buildNeuronStr layerIdx neuronIdx (n:nList) r1 r2 = buildNeuronStr layerIdx nextIdx nList tmp1 tmp2
	where
	nextIdx = neuronIdx + 1
	neuronStr = printf "N[%d][%d]" (layerIdx::Int) (neuronIdx::Int)
	stateStr = printf "%f" ((state n) :: Double)
	tmp1 = r1 ++ [neuronStr]
	tmp2 = r2 ++ [stateStr]


getMaxLayerSize :: [[String]] -> Int -> Int
getMaxLayerSize [] max = max
getMaxLayerSize (curList:listOfList) max = getMaxLayerSize listOfList curMax
	where
	curMax | length curList > max = length curList
		   | otherwise = max


concateAllNeuron :: Int -> [[String]] -> [String] -> [String]
concateAllNeuron max [] res = res
concateAllNeuron max (l:lists) res = concateAllNeuron max restList tmp
	where
	restList | length lists > 0 = tail lists
			 | otherwise = []
	headList | length lists > 0 = head lists
			 | otherwise = []
	tmp = res ++ (concateNeuronStr max l headList)

concateNeuronStr :: Int -> [String] -> [String] -> [String]
concateNeuronStr maxNeurons nStrList sStrList = result
	where
	-- needed for calcinc the spaceL if this layer has less neurons
	maxDiff = maxNeurons - length nStrList
	
	maxDiffHalf | (maxDiff `mod` 2) == 0 = (maxDiff `div` 2)
				| otherwise = (maxDiff + 1) `div` 2
	
	-- spacer for the first neuron on the left side
	spacerL | maxDiff == 1 = buildSpacer 7
			| maxDiff > 0 && maxDiff `mod` 2 == 0 = buildSpacer (maxDiffHalf * 12)
			| maxDiff > 0 && maxDiff `mod` 2 == 1 = buildSpacer (maxDiffHalf * 9)
			| otherwise = buildSpacer 0
	
	-- spacer between 2 neurons
	spacerN = buildSpacer 5
	
	-- needet for extra spaces if state line below is shorter. 2nd map: length of neuron string 
	spacerS = zipWith (\n str -> buildSpacer (n - length str)) (map (\n -> length n) nStrList) sStrList
			
	-- row1: neurons, row2: states of neurons
	row1 = spacerL ++ concat (map (\n -> n ++ spacerN) nStrList) ++ "\n"
	row2 = spacerL ++ concat (zipWith (\sp n -> n ++ spacerN ++ sp) spacerS sStrList) ++ "\n\n"
	result = [(row1 ++ row2)]

buildSpacer :: Int -> String
buildSpacer size = concat (replicate size " ")