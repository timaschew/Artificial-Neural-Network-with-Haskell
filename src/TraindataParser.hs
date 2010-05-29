import System.IO
import Data.Char
import Data.List
import Trainingdata

main :: IO ()
main = do
	-- read files
	input <- readFile "01.train"
	
	putStrLn "parsing traindata file..."
	
	let traindata = parseTrainData (lines input) [] []
	let inputList = fst traindata	-- [[input1],[input2],...]
	let outputList = snd traindata	-- [[output1],[output2],...]

	putStrLn "Done."

	
-- input is the file input (whole string)
getTrainingdata :: String -> Int -> Trainingdata
getTrainingdata input steps = result where
	tupel = parseTrainData (lines input) [] []
	ti = map (\x -> makeDoubleList x) (fst tupel)
	to = map (\x -> makeDoubleList x) (snd tupel)
	result = Trainingdata steps ti to

-- input is the file input (whole string)
makeAlterningTrainingdata :: String -> Int -> Trainingdata
makeAlterningTrainingdata input steps = result where
	tupel = parseTrainData2 (lines input) [] []
	ti = map (\x -> makeDoubleList x) (fst tupel)
	to = map (\x -> makeDoubleList x) (snd tupel)
	result = Trainingdata steps ti to
		
makeDoubleList :: [Int] -> [Double]
makeDoubleList intList = doubleList where
	doubleList = map fromIntegral intList -- conversion: int -> double with fromIntegral
	

-- result tupel: ( [[input1],[input2],...] , [[output1],[output2],...] )
parseTrainData :: [String] -> [[Int]] -> [[Int]] -> ([[Int]], [[Int]])
parseTrainData [] inputList outputList = (inputList, outputList)
parseTrainData (l:lines) inputList outputList = parseTrainData lines inputList' outputList' where
	tupel = getLineData l
	inputList' | isDataLine l = inputList ++ [(fst tupel)] 
			   | otherwise = inputList -- add nothing
	outputList' | isDataLine l = outputList ++ [(snd tupel)]
			    | otherwise = outputList -- add nothing

-- result tupel: ( [[input1],[input2],...] , [[output1],[output2],...] )
parseTrainData2 :: [String] -> [[Int]] -> [[Int]] -> ([[Int]], [[Int]])
parseTrainData2 [] inputList outputList = (inputList, outputList)
parseTrainData2 (l:lines) inputList outputList = parseTrainData2 (reverse lines) inputList' outputList' where
	tupel = getLineData l
	inputList' | isDataLine l = inputList ++ [(fst tupel)] 
			   | otherwise = inputList -- add nothing
	outputList' | isDataLine l = outputList ++ [(snd tupel)]
			    | otherwise = outputList -- add nothing

-- a traindata line contains input and output values separated with '-'
-- format: "123 456 - 789"
-- split => ("123 456 ", " 789")
-- => input: [123, 456], output: [789]
getLineData :: String -> ([Int], [Int])
getLineData line = result where
	idx = '-' `elemIndices` line -- find the separator
	tuple = splitAt (head idx) (delete '-' line)	-- split the line in a tuple: (input, output)
	inputList = map stringToInt (words $ fst tuple)
	outputList = map stringToInt (words $ snd tuple)
	result = (inputList, outputList)
	--result = ((words $ fst tuple), (words $ snd tuple))

-- converts a string to int
stringToInt :: String -> Int
stringToInt str = read str::Int

-- returns False if the line starts with an '#' or '--' or is empty
isDataLine :: String -> Bool
isDataLine line = result where
	tmp = dropWhile isSpace line -- eliminate leading spaces
	result | (length tmp == 0) = False
		   | (length tmp > 1) && ("#" `isPrefixOf` tmp) = False
		   | (length tmp > 2) && ("--" `isPrefixOf` tmp) = False
		   | otherwise = True
