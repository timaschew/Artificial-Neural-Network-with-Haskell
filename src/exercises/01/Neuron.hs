import System.IO

--
-- read file "neuron" and generate a Neuron instance
--

makeDouble :: String -> Double
makeDouble s = read s :: Double

data Neuron = Neuron {
	treshold :: Double, 
	weights :: [Double]
	} deriving (Show)

main :: IO ()
main = do
	input <- readFile "neuron"
	let list = lines input

	let comment = list !! 0
	let treshold = makeDouble (list !! 1)
	let weights = map makeDouble $ drop 2 list :: [Double]

	let n = Neuron treshold weights
	putStrLn $ "Datei eingelesen (" ++ comment ++ ")"
	print n
	
