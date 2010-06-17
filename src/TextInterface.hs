module TextInterface where
import Data.Char
import System.IO
import System.Directory
import Data.List

import Neuron
import Trainingdata
import TraindataParser
import Backpropagation
import Utils
import Config

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
	input <- readFile "data/trainingdata/txt/de_en_02.train"
	let td = getTrainingdata input 10
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

-- hidden
b2_0 = biasNeuron { weights = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] }
n2_1 = defaultNeuron { weights = 
[-0.966, 1.289, 0.467, 0.624, -1.199, 0.364, -1.37, 0.56, -1.503, -0.724, 0.429, -0.377, 0.521, -1.632, 1.007] }
n2_2 = defaultNeuron { weights = 
[-1.185, 1.029, 0.719, -0.507, 0.627, -1.296, 0.243, 1.172, -0.038, 0.866, -0.653, 0.571, -0.427, 1.548, -1.379] }
n2_3 = defaultNeuron { weights = 
[0.012, 1.944, 0.432, 1.187, -0.592, -0.548, -0.96, -1.824, 0.531, -1.325, 1.984, 1.3, 0.964, -0.805, 1.926] }
n2_4 = defaultNeuron { weights = 
[-0.5, 0.914, -0.672, -1.423, -0.459, 0.1, -1.184, -0.482, 0.075, 1.151, -0.435, -1.795, 1.757, 1.264, -0.057] }
n2_5 = defaultNeuron { weights = 
[-0.415, -0.895, 0.065, 0.448, -1.689, 0.307, 1.455, -1.346, 1.771, 0.442, -0.024, 1.482, -0.752, 0.8, -1.271] }
n2_6 = defaultNeuron { weights = 
[-0.203, -1.815, -1.119, -0.637, 1.863, 0.521, -0.49, 1.191, -1.551, 0.296, -1.42, -0.237, -1.105, -1.511, -0.789] }
n2_7 = defaultNeuron { weights = 
[1.603, -0.334, 0.114, 0.326, -1.983, 1.341, -1.294, 0.913, -0.563, 0.317, -0.652, 0.034, 0.806, 0.408, 0.215] }
n2_8 = defaultNeuron { weights = 
[-1.929, 0.542, -0.796, 0.375, -1.6, 1.605, -0.732, 0.579, 1.113, 0.015, -1.356, -1.992, 0.285, 1.944, -0.249] }
n2_9 = defaultNeuron { weights = 
[-0.714, 1.306, -1.686, -1.85, 0.645, 1.83, 0.611, 1.039, 1.245, 0.088, 1.009, 1.271, -1.68, 1.359, 0.356] }
n2_10 = defaultNeuron { weights = 
[1.292, 1.675, -1.493, -1.559, -1.999, 0.323, 0.869, 0.562, 1.917, -0.101, -0.756, -0.349, -1.711, 0.762, -1.132] }
-- output
n3_1 = defaultNeuron { weights = [0.879, 1.2, -1.915, -0.723, -0.297, -1.229, -1.737, 1.33, 1.161, -0.24, -0.438] }
n3_2 = defaultNeuron { weights = [-0.279, 0.2, 0.415, -2.713, 0.297, -0.229, -2.237, -1.83, 0.81, -1.84, 0.138] }

network = [[b1_0, n1_1, n1_2, n1_3, n1_4, n1_5, n1_6, n1_7, n1_8, n1_9, n1_10, n1_11, n1_12, n1_13, n1_14], [b2_0, n2_1, n2_2, n2_3, n2_4, n2_5, n2_6, n2_7, n2_8, n2_9, n2_10], [n3_1, n3_2]]

demo :: Network -> TrainData -> [Double]
demo net inputData = result where
	inputted = setTrainToInputLayer net inputData
	forwarded = forwardPass inputted []
	result = makeStateListOfLayer (last forwarded) []

trainNet :: Network -> Int -> Trainingdata -> Network
trainNet net 0 tdata = net
trainNet net steps tdata = trainNet trained (steps-1) tdata where
	trained = genericTraining net tdata 0

path = (dataPath ++ "traindata/txt/raw/")

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
	let converted = w
	
	exportData w fileName outputResult
	
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
prepareData :: [String] -> String -> String
prepareData d outputResult = result where 
	--bla = map (makeComment) d
	--strListList = map (map (show)) d -- convert Int to String (for using unwords)
	withComments = map (makeCommentAndOutput outputResult) d 
	concated = map unwords withComments -- concat list with whitespaces
	prepared = map (++ "\n") concated -- add newline after each list element
	maxWordLength = maximum (map length d)
	-- print list as one string with max word length
	result = "# MAXIMUM: "++ (show maxWordLength)  ++ "\n" ++ concat prepared

-- produce a comment with the reconverted string and the teaching output value
makeCommentAndOutput :: String -> String -> [String] 
makeCommentAndOutput output slist = tmp where
	soundexBin = soundexToBitStream (soundexAlgo slist)
	-- build comment with real string
	-- add converted char int valueus (remove first whitespace)
	-- add teaching output value
	tmp = ["# " ++ slist ++ "\n"] ++ [soundexBin] ++ ["- " ++ output]

str2Soundex :: String -> String
str2Soundex str = soundexToBitStream (soundexAlgo str)

str2ST str = map readDigit (words (str2Soundex str))

readDigit x = read x :: Double

soundexToBitStream :: Soundex -> String
soundexToBitStream (c, i1, i2, i3) = tmp where
	bin1 = dec2binLeading ((ord (toLower c)) - 97) 5
	bin2 = dec2binLeading i1 3
	bin3 = dec2binLeading i2 3
	bin4 = dec2binLeading i3 3
	tmp = unwords (bin1 ++ bin2 ++ bin3 ++ bin4)


-- ignore all chars except a-z and white spaces
filterRealChars :: String -> String
filterRealChars s = stripNotRealChars where
	stripNotRealChars = filter (\x -> isRealChar x) (map toLower s)
	isRealChar c = ((ord c) >= 97 && (ord c) <= 122) || (ord c ) == 32


{--
Ziffer 	Repräsentierte Buchstaben
0	- / no letter
1 	B, F, P, V
2 	C, G, J, K, Q, S, X, Z
3 	D, T
4 	L
5 	M, N
6 	R
--}

type Soundex = (Char, Int, Int, Int)

skipDoubleChars :: [Char] -> Char -> [Char] -> [Char]
skipDoubleChars [x] oldChar c = r where
	r	| (x == oldChar) = c
		| otherwise = c ++ [x]
skipDoubleChars (x:l) oldChar c = skipDoubleChars l x r where
	tmp	| (oldChar == x) = []
		| otherwise = [x]
	r = c ++ tmp

ignoreDoubleChars :: String -> String
ignoreDoubleChars str = skipDoubleChars str '\NUL' []


soundexAlgo :: String -> Soundex
soundexAlgo str = r where
	cleaned = ignoreDoubleChars str
	s = skipVocals (tail cleaned)
	char0 = head cleaned
	i1	| ((length s) >= 1) = mapSoundexCode (s !! 0)
		| otherwise = 0
	i2	| ((length s) >= 2) = mapSoundexCode (s !! 1)
		| otherwise = 0
	i3 	| ((length s) >= 3) = mapSoundexCode (s !! 2)
		| otherwise = 0
	r = ((head str),i1,i2,i3)
	
skipVocals :: String -> String
skipVocals str = filter (\x -> x /= 'a' && x /= 'e' && x /= 'i' && x /= 'o' && x /= 'u' &&
				x /= 'h' && x /= 'w' && x /= 'y') str

mapSoundexCode :: Char -> Int
mapSoundexCode x = r where
	r	| x == 'b' || x == 'f' || x == 'p' || x == 'v' = 1
		| x == 'c' || x == 'g' || x == 'j' || x == 'k' || x == 'q' || x == 's' || x == 'x' || x == 'z' = 2
		| x == 'd' || x == 't' = 3
		| x == 'l' = 4
		| x == 'm' || x == 'n' = 5
		| x == 'r' = 6
		| otherwise = 9

-- replace 2nd arg with 3rd arg in 1st arg
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
