import System.IO

data Neuron = Neuron {	treashold :: Double
					, weights :: [Double]
					} deriving (Show)

main = do

	inh <- openFile "neuron" ReadMode
	let l = [] :: [Double]
	readLineLoop inh l
	-- print empty list
	print l
	
	-- don'work yet
	--let threshold = (lines contents) !! 1
	--let n = Neuron threshold l
	--print "fertig"
	
readLineLoop :: Handle -> [Double] -> IO ()
readLineLoop inh list = do 
	is_eof <- hIsEOF inh
	if is_eof
		then return()
		else do
			inpStr <- hGetLine inh
			let val = read inpStr :: Double
			putStrLn inpStr
			readLineLoop inh (val:list)

