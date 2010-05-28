module TextInterface where
import Data.Char
import System.IO
import System.Directory

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
