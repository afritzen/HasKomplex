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
	    _   -> main
	main

-- processes a single-number-input and performs calculations
processDoubleOperation :: IO ()
processDoubleOperation = do
	putStrLn "Please enter two complex numbers:"
	fstNumberReal <- getLine
	fstNumberImg <- getLine
	sndNumberReal <- getLine
	sndNumberImg <- getLine
	let fstNumber = toComplex (read fstNumberReal) (read fstNumberImg)
	let sndNumber = toComplex (read sndNumberReal) (read sndNumberImg)
	putStrLn "Choose an operation (+, -, *, /, abs):"
	opCode <- getLine
	case opCode of
	  "+" -> putStrLn $ complexToString $ complexAdd fstNumber sndNumber
	  "-" -> putStrLn $ complexToString $ complexSub fstNumber sndNumber
	  "*" -> putStrLn $ complexToString $ complexMult fstNumber sndNumber
	  "/" -> putStrLn $ complexToString $ complexDiv fstNumber sndNumber
	  "q" -> main
	  _   -> putStrLn "Invalid operation code!" >> processDoubleOperation
	main
		
-- processes a two-number-input and performs calculations
processSingleOperation :: IO ()
processSingleOperation = do
	putStrLn "Please enter a complex number:"
	numberReal <- getLine
	numberImg <- getLine
	let number = toComplex (read numberReal) (read numberImg)
	putStrLn "Choose an operation (k, p):"
	opCode <- getLine
	case opCode of
		"k" -> putStrLn $ complexToString $ complexConjugate number
		"p" -> putStrLn "IMPLEMENT!"
		"q" -> main
		_   -> putStrLn "Invalid operation code!" >> processSingleOperation
	main
 