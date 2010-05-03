import System.IO

--
-- This is only the file reader
--

main = do
	-- alternative file reading
--	handle <- openFile "neuron" ReadMode
--	contents <- hGetContents handle
--	-- read lines
--	hClose handle

	contents <- readFile "neuron"
	putStr contents

	let allLines = lines contents
	putStr $ "line[0] = " ++ allLines !! 0 ++ "\n"
	putStr $ "line[1] = " ++ allLines !! 1 ++ "\n"
	putStr $ "line[2] = " ++ allLines !! 2 ++ "\n"

-- getL allLines index = allLines !! 0
-- printL singleLine index = putStr "line[" ++ index ++ "] = " + singleLine
-- printAllLines allLines = printL allLines TODO
