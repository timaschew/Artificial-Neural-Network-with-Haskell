import System.IO
import Data.Char
import Data.List

-- read in the bible text and write all words found (no duplicates, braces, dots etc.) newline separated into a file.

main :: IO ()
main = do
	-- read files
	inputEN <- readFile "bible_en"
	inputDE <- readFile "bible_de"
	
	-- split text into a list of words and eliminate duplicates
	let listEN = sort (words inputEN)
	let listDE = sort (words inputDE)
		
	-- write filtered words into file
	writeFile "words_en" $ unlines (getFilteredWords listEN)
	writeFile "words_de" $ unlines (getFilteredWords listDE)
			
	putStr "Done\n"	

-- returns a list of filtered words. note: filterWord can return a "", which has to be filtered 
getFilteredWords :: [String] -> [String]
getFilteredWords wordList = result where
	filteredWordList = map (\w -> filterWord w) wordList
	eliminatedBlank = filter (/= "") filteredWordList
	result = sort (nub eliminatedBlank) -- nub function is most expensive in this script

-- skips all non letter of the word and make all chars towercase
filterWord w = result where
	onlyLetters = filter isLetter w
	result = map toLower onlyLetters
	
