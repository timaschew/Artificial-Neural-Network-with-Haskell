module Test where


testTrain :: IO ()
testTrain = do
	-- read files
	input <- readFile (dataPath ++ "traindata/txt/de_01.train")
	
	putStrLn "parsing traindata file..."
	
	let traindata = parseTrainData (lines input) [] []
	let inputList = fst traindata	-- [[input1],[input2],...]
	let outputList = snd traindata	-- [[output1],[output2],...]

	putStrLn "Done."

	
-- read in the topology file and do a prettyPrint on console
testTopo = do
	-- read file
	input <- readFile (dataPath ++ "topology")
	
	putStrLn "parsing topology file..."
	randNums <- mapM (\x -> getRandNum) [1..countNeededRandNums input]
	let topology = getTopology input randNums
	putStrLn "Done."
	prettyPrint topology
	