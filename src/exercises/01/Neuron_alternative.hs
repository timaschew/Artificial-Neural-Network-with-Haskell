import System.IO -- need to run as GHCi session (in eclipse)

data Neuron = Neuron {
	treashold :: Double,
	weights :: [Double]
	} deriving (Show)

main = do

	inh <- openFile "neuron" ReadMode
	let l = [] :: [Double]
	let listresult = readLineLoop inh l
	
	-- could not cast from IO [Double] to [Double]
	print listresult
	
	let n = Neuron 0.5 listresult
	print n
	

readLineLoop inh list = do 
	is_eof <- hIsEOF inh
	if is_eof
		-- could not use "return list" => errors
		then return list
		else do
			inpStr <- hGetLine inh
			let val = read inpStr :: Double
			readLineLoop inh (val:list)

