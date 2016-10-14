{-- Main module for HasKomplex, processes user input, performs desired calculations and 
prints the result to the commandl line. --}

module Main where

import Complex
import System.Exit (exitSuccess)

main :: IO ()
main = do 
	putStrLn "\n**WELCOME TO HasKomplex!**"
	putStrLn "\nWhat would you like to do ('s' - single operation, 'd' - double operation, \n'h' for help, 'q' to quit)?"
	choice <- getLine
	case choice of
	    "s" -> processSingleOperation
	    "d" -> processDoubleOperation
	    "q" -> putStrLn "Qutting ..." >> exitSuccess
	    "h" -> putStrLn "\nTo enter a complex number, type 'Complex' followed by \nthe real and imaginary part as separated numbers.\nNegative numbers must be enclosed in brackets."
	    _   -> main
	main

-- processes a single-number-input and performs calculations
processDoubleOperation :: IO ()
processDoubleOperation = do
	putStrLn "\nPlease enter two complex numbers:"
	fstNumberInput <- getLine
	sndNumberInput <- getLine
	let fstNumber = read fstNumberInput
	let sndNumber = read sndNumberInput
	putStrLn "\nChoose an operation (+, -, *, /, abs):"
	opCode <- getLine
	case opCode of
	  "+" -> putStrLn "\n" >> (putStrLn $ complexToString $ complexAdd fstNumber sndNumber) 
	  "-" -> putStrLn "\n" >> (putStrLn $ complexToString $ complexSub fstNumber sndNumber)
	  "*" -> putStrLn "\n" >> (putStrLn $ complexToString $ complexMult fstNumber sndNumber)
	  "/" -> putStrLn "\n" >> (putStrLn $ complexToString $ complexDiv fstNumber sndNumber)
	  "q" -> main
	  _   -> putStrLn "Invalid operation code!" >> processDoubleOperation
	main
		
-- processes a two-number-input and performs calculations
processSingleOperation :: IO ()
processSingleOperation = do
	putStrLn "\nPlease enter a complex number:"
	numberInput <- getLine
	let number = read numberInput
	putStrLn "\nChoose an operation (k, p):"
	opCode <- getLine
	case opCode of
		"k" -> putStrLn "\n" >> (putStrLn $ complexToString $ complexConjugate number)
		"p" -> putStrLn "\n" >> (putStrLn $ printPolar $ complexToPolar number)
		"q" -> main
		_   -> putStrLn "Invalid operation code!" >> processSingleOperation
	main
 