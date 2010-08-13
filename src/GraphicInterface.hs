module GraphicInterface where


import System.Directory
import Data.List


{--
example path: "data/traindata/img/10_12/big_numbers/0.pgm"

readPPMFile "data/traindata/img/10_12/big_numbers/0.pgm"
--}

readPPMFile :: String -> IO [Double]
readPPMFile fn = do
	inp <- readFile fn
	let lineList = lines inp
	let res = words (lineList !! 1)
	let width = read (res !! 0) :: Int
	let height = read (res !! 1) :: Int
	let pixels = concat $ map words (drop 4 lineList)
	return (map normalize pixels)
	
normalize x = norm where
	z = read x :: Double
	norm = z / 255


-- returns a list of inputvalues from the given ppm folder.
-- reads every ppm and generates its inputValues from the pixels values
getPPMInput :: String -> IO [[Double]]
getPPMInput path = do
	let fullPath = path--dataPath ++ path
	nameList <- getPgmList fullPath
	fileList <- mapM readPPMFile (map (\x -> fullPath ++ x) nameList)

	return fileList


-- scans a directory for ppm files and returns a list of their filenames
getPgmList :: String -> IO [String]
getPgmList path = do
	dirContent <- getDirectoryContents path
	let pgmList = filter (isSuffixOf ".pgm") dirContent
	return $ sort pgmList
	