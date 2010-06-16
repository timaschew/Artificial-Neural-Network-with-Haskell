module Menu where

import Text.Printf
import Neuron
import Utils
import Trainingdata
import Backpropagation


menuLoop goodNet = do
	printMenu
	action <- getLine
	
	-- choose action
	let doAction | action == "1" = prettyPrint goodNet
				 | action == "2" = dummyAction
				 | action == "3" = dummyAction
				 | action == "4" = onWork goodNet
				 | action == "5" = dummyAction				 
				 | action == "0" = return()
				 | otherwise = onError
	
	-- invoke action
	doAction
	if action /= "0"
		then menuLoop goodNet
		else putStrLn "\nGoodbye!"

	
printMenu :: IO ()
printMenu = do
	printf "-----------------------\n"
	printf "--  Main Menu\n"
	printf "-----------------------\n"
	printf "[1] show topology\n"
	printf "[2] show traindata file\n"
	printf "[3] train ann\n"
	printf "[4] work\n"
	printf "[0] exit\n"
	printf "-----------------------\n"
	printf "select one action: "
	
onError :: IO ()	
onError = do
	putStrLn "\nunknown action!\n"
	--main

onWork :: Network -> IO ()
onWork net = do
	printf "\n[work] please enter some ann input (e.g: 1 0): \n"
	inputStr <- getLine
	-- TODO: check input for numbers or/and catch exception	
	let values = map (\w -> read w::Double) (words inputStr)
	
	if length values == 0
		then return()	-- leave onWork loop
		else do
			 putStrLn "please wait..."
			 let resultList = work net values
			 putStr "["
			 mapM_ (\v -> printf "%.3f " v) resultList
			 putStr "]"			 
			 --print $ work net values
			 onWork net -- stay in onWork loop

dummyAction :: IO()
dummyAction = do
	putStrLn "\nOooops! This action is not implemented yet.\n"
	