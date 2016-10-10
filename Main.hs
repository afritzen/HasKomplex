{-- Main module for HasKomplex, processes user input, performs desired calculations and 
prints the result to the commandl line. --}

module Main where

import Complex
import System.Exit (exitSuccess)

main :: IO ()
main = do 
	putStrLn "**WELCOME TO HasKomplex!**"
	putStrLn "What would you like to do?"
	choice <- getLine
	case choice of
		"s" -> processSingleOperation
		"d" -> processDoubleOperation
		"q" -> exitSuccess
		"h" -> putStrLn "--HELPTEXT HERE--"
		otherwise -> main
	main

-- processes a single-number-input and performs calculations
processSingleOperation :: IO ()
processSingleOperation = do
			  putStrLn "TODO"

-- processes a two-number-input and performs calculations
processDoubleOperation :: IO ()
processDoubleOperation = do
			  putStrLn "TODO"
	
