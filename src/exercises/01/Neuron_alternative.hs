import System.IO

data Neuron = Neuron {	treashold :: Double
					, weights :: [Double]
					} deriving (Show)

main = do

	inh <- openFile "neuron" ReadMode
	let l = [] :: [Double]
	readLineLoop inh l
	
	-- list is empty! do not save the calculations of readLineLoop in l
	print l
	
readLineLoop :: Handle -> [Double] -> IO ()
readLineLoop inh list = do 
	is_eof <- hIsEOF inh
	if is_eof
		-- could not use "return list" => errors
		then return()
		else do
			inpStr <- hGetLine inh
			let val = read inpStr :: Double
			putStrLn inpStr
			readLineLoop inh (val:list)

