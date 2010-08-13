module TraindataParser where
import System.IO
import Data.Char
import Data.List
import Trainingdata

	
-- input: the file input (whole string)
getTrainingdata :: String -> Trainingdata
getTrainingdata input = result where
	tupel = parseTrainData (lines input) [] []
	ti = map (\x -> makeDoubleList x) (fst tupel)
	to = map (\x -> makeDoubleList x) (snd tupel)
	result = Trainingdata (length ti) ti to

		
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
