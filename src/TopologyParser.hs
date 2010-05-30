module TopologyParser where
import Neuron
import Utils
import Data.Char
import Data.List

-- read in the topology file and do a prettyPrint on console
main :: IO ()
main = do
	-- read file
	input <- readFile "../data/topology"
	
	putStrLn "parsing topology file..."
	
	let topology = getTopology input
	
	putStrLn "Done."
	prettyPrint topology	


-- param: whole file input str
-- returns the generated Network
getTopology :: String -> Network
getTopology input = result where
	tupleList = parseTopology (lines input) []
	
	----------------------------------------------
	-- TODO: build / generate topo from tupleList
	----------------------------------------------
	
	result = [[(defaultNeuron)]]	-- TODO: neuron network
	
	
parseTopology :: [String] -> [(Int, Bool)] -> [(Int, Bool)]
parseTopology [] tup = tup
parseTopology (l:lines) tup = parseTopology lines tup' where
	hasData = isDataLine l	
	lineData = if hasData
					then getLineData l	
					else (0, False)
	tup' | hasData = tup ++ [(fst lineData, snd lineData)]
		 | otherwise = tup	-- nothing to add
	
	
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
