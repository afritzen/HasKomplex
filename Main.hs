{-- Main module for HasKomplex, processes user input, performs desired calculations and 
prints the result to the commandl line. --}

module Main where

import Complex

main :: IO ()
main = do 
	putStrLn "**WELCOME TO HasKomplex!**"
	-- TODO: distinguish between one and two numbers and add operations!
	putStrLn "Enter two numbers and an operation you would like to perform:"
	firstNumberReal <- getLine
	firstNumberImg <- getLine
	secondNumberReal <- getLine
	secondNumberImg <- getLine
	-- TODO: check numbers for invalid input!
	operationCode <- getLine
	let complexFirst = toComplex (read firstNumberReal) (read firstNumberImg)
	let complexSecond = toComplex (read secondNumberReal) (read secondNumberImg)
	putStrLn $ show complexFirst
	putStrLn $ show complexSecond
	case operationCode of 
		"+" -> putStrLn $ complexToString $ complexAdd complexFirst complexSecond 
		"-" -> putStrLn $ complexToString $ complexSub complexFirst complexSecond
		"*" -> putStrLn $ complexToString $ complexMult complexFirst complexSecond
		"/" -> putStrLn $ complexToString $ complexDiv complexFirst complexSecond
		-- TODO: add polarform!
		"h" -> putStrLn "-- HELPTEXT HERE --"
		"q" -> putStrLn "Quitting ..."
		otherwise -> putStrLn "Invalid operation! Press 'h' for help."
	main
	