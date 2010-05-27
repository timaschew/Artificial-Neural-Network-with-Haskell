module TextInterface where
import Data.Char

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

-- make lower case
stringToIntList :: String -> String
stringToIntList s = intList where	
	intList = map (\x -> ord (toLower x)) s

-- replace 2nd arg with 3rd arg in 1st arg
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)



