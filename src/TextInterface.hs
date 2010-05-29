module TextInterface where
import Data.Char
import System.IO
import System.Directory

import Neuron
import Trainingdata
import TraindataParser
import Backpropagation
import Utils

{--
Characters to skip
	[0-9]
	.
	,
	;
	"
	»
	«
	”
	”
	“
	’
	$
	?
	‘
	
Questions / Rules for mapping strings on neurons
- max size of chars ?
- min size of chars ?
- allow words joining with a dash -

--}

-- text network 19 input neurons max of en/de word length

{--
	-- workflow to test text training
	input <- readFile "data/traindata/txt/de_en_01.train"
	-- file has 2546 words
	let td = getTrainingdata input 2546
	-- or alternating trainingdata
	let td2 = makeAlterningTrainingdata input 2546
	
	let goodNet = trainNet network 10 td
	
	-- string: "laune"
	demo goodNet [108,97,117,110,101]
	
	-- string: "empty"
	demo goodNet [101,109,112,116,121]
--}

b1_0 = biasNeuron 
n1_1 = defaultNeuron
n1_2 = defaultNeuron
n1_3 = defaultNeuron
n1_4 = defaultNeuron
n1_5 = defaultNeuron
n1_6 = defaultNeuron
n1_7 = defaultNeuron
n1_8 = defaultNeuron
n1_9 = defaultNeuron
n1_10 = defaultNeuron
n1_11 = defaultNeuron
n1_12 = defaultNeuron
n1_13 = defaultNeuron
n1_14 = defaultNeuron
n1_15 = defaultNeuron
n1_16 = defaultNeuron
n1_17 = defaultNeuron
n1_18 = defaultNeuron
n1_19 = defaultNeuron
-- hidden
b2_0 = biasNeuron { weights = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] }
n2_1 = defaultNeuron { weights = 
[-0.966, 1.289, 0.467, 0.624, -1.199, 0.364, -1.37, 0.56, -1.503, -0.724, 0.429, -0.377, 0.521, -1.632, 1.007, -1.622, 1.079, -0.827, 1.135, -1.914] }
n2_2 = defaultNeuron { weights = 
[-1.185, 1.029, 0.719, -0.507, 0.627, -1.296, 0.243, 1.172, -0.038, 0.866, -0.653, 0.571, -0.427, 1.548, -1.379, -0.392, 0.599, -0.42, 0.708, 0.547] }
n2_3 = defaultNeuron { weights = 
[0.012, 1.944, 0.432, 1.187, -0.592, -0.548, -0.96, -1.824, 0.531, -1.325, 1.984, 1.3, 0.964, -0.805, 1.926, 1.709, -1.516, 1.294, -1.238, -1.541] }
n2_4 = defaultNeuron { weights = 
[-0.5, 0.914, -0.672, -1.423, -0.459, 0.1, -1.184, -0.482, 0.075, 1.151, -0.435, -1.795, 1.757, 1.264, -0.057, -0.901, -1.43, 1.116, 1.67, 0.44] }
n2_5 = defaultNeuron { weights = 
[-0.415, -0.895, 0.065, 0.448, -1.689, 0.307, 1.455, -1.346, 1.771, 0.442, -0.024, 1.482, -0.752, 0.8, -1.271, -1.342, -0.123, 0.085, 0.075, -0.466] }
n2_6 = defaultNeuron { weights = 
[-0.203, -1.815, -1.119, -0.637, 1.863, 0.521, -0.49, 1.191, -1.551, 0.296, -1.42, -0.237, -1.105, -1.511, -0.789, -1.716, 0.648, 1.063, 1.826, -0.178] }
n2_7 = defaultNeuron { weights = 
[1.603, -0.334, 0.114, 0.326, -1.983, 1.341, -1.294, 0.913, -0.563, 0.317, -0.652, 0.034, 0.806, 0.408, 0.215, -1.222, 1.499, 0.738, -1.445, 0.415] }
n2_8 = defaultNeuron { weights = 
[-1.929, 0.542, -0.796, 0.375, -1.6, 1.605, -0.732, 0.579, 1.113, 0.015, -1.356, -1.992, 0.285, 1.944, -0.249, 1.654, 0.475, 0.424, 1.222, -1.054] }
n2_9 = defaultNeuron { weights = 
[-0.714, 1.306, -1.686, -1.85, 0.645, 1.83, 0.611, 1.039, 1.245, 0.088, 1.009, 1.271, -1.68, 1.359, 0.356, 1.338, 0.191, -1.061, 1.411, 0.494] }
n2_10 = defaultNeuron { weights = 
[1.292, 1.675, -1.493, -1.559, -1.999, 0.323, 0.869, 0.562, 1.917, -0.101, -0.756, -0.349, -1.711, 0.762, -1.132, -0.473, 1.827, -0.287, 0.863, -1.583] }
-- output
n3_1 = defaultNeuron { weights = [0.879, 1.2, -1.915, -0.723, -0.297, -1.229, -1.737, 1.33, 1.161, -0.24, -0.438] }

network = [[b1_0, n1_1, n1_2, n1_3, n1_4, n1_5, n1_6, n1_7, n1_8, n1_9, n1_10, n1_11, n1_12, n1_13, n1_14, n1_15, n1_16, n1_17, n1_18, n1_19], [b2_0, n2_1, n2_2, n2_3, n2_4, n2_5, n2_6, n2_7, n2_8, n2_9, n2_10], [n3_1]]

demo :: Network -> TrainData -> [Double]
demo net inputData = result where
	inputted = setTrainToInputLayer net inputData
	forwarded = forwardPass inputted []
	result = makeStateListOfLayer (last forwarded) []
	
trainNet :: Network -> Int -> Trainingdata -> Network
trainNet net 0 tdata = net
trainNet net steps tdata = trainNet trained (steps-1) tdata where
	trained = genericTraining net tdata 0

path = "data/traindata/txt/raw/"

-- import text from a file an generate a train file for the ANN
startImport :: IO ()
startImport = do
	curPath <- getCurrentDirectory
	putStrLn $ ("Pfad: " ++ (curPath) ++ "/" ++ path)
	putStrLn "Bitte Dateiname zum importieren angeben"
	fileName <- getLine
	putStrLn "Imoprtiere ..."
	let r = importText fileName
	r2 <- r
	putStrLn "Import abgeschlossen"
	
	let r3 = filterRealChars r2
	let w = words r3
	let len = length w
	putStrLn $ ((show len) ++ " Woerter gefunden")
	print w
	
	putStrLn "Bitte Kodierung fuer Sprache der Trainingdaten waehlen"
	outputResult <- getLine
	
	putStrLn "Konvertierung erfolgt ..."
	let converted = map stringToIntList w 
	
	exportData converted fileName outputResult
	
	putStrLn "TrainFinished"
	
-- import text from file and return a String IO
importText :: String -> IO String
importText s = do
	file <- readFile (path ++ s)
	return file

-- export data to fileName
exportData trainData fileName outputResult = do
	let output = prepareData trainData outputResult
	let fileNameNew = reverse (drop 4 (reverse fileName)) ++ ".train"
	writeFile (path ++ "../" ++ fileNameNew) output

-- make string of converted data for writing to a file
prepareData :: [[Int]] -> String -> String
prepareData d outputResult = result where 
	--bla = map (makeComment) d
	strListList = map (map (show)) d -- convert Int to String (for using unwords)
	withComments = map (makeCommentAndOutput outputResult) strListList 
	concated = map unwords withComments -- concat list with whitespaces
	prepared = map (++ "\n") concated -- add newline after each list element
	maxWordLength = maximum (map length d)
	-- print list as one string with max word length
	result = "# MAXIMUM: "++ (show maxWordLength)  ++ "\n" ++ concat prepared

-- produce a comment with the reconverted string and the teaching output value
makeCommentAndOutput :: String -> [String] -> [String] 
makeCommentAndOutput output slist = tmp where
	digit x = read x :: Int -- read as Int
	digitList = map digit slist -- for each string element ("97")
	str = map chr digitList -- reconvert from [Int] into String
	
	-- build comment with real string
	-- add converted char int valueus (remove first whitespace)
	-- add teaching output value
	tmp = ["# " ++ str ++ "\n"] ++ slist ++ ["- " ++ output]


-- converts a string into a char int values list (and lower case)
-- map a to 97
-- ...
-- map z to 122
stringToIntList :: String -> [Int]
stringToIntList s = intList where	
	intList = map (\x -> ord (toLower x)) s
	
	
-- ignore all chars except a-z and white spaces
filterRealChars :: String -> String
filterRealChars s = stripNotRealChars where
	stripNotRealChars = filter (\x -> isRealChar x) (map toLower s)
	isRealChar c = ((ord c) >= 97 && (ord c) <= 122) || (ord c ) == 32


-- replace 2nd arg with 3rd arg in 1st arg
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
