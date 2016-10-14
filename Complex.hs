{-- A module for complex numbers providing all common operations. --}

module Complex where

import Data.Maybe

data Complex = Complex Int Int
			   deriving (Show, Eq, Ord, Read)

-- complex number with positive real and negative imaginary part 
complex1 :: Complex
complex1 = Complex 2 (-3)

-- complex number with negative real and positive imaginary part 
complex2 :: Complex
complex2 = Complex (-4) 2

-- takes two numbers and wraps them to the complex type
toComplex :: Int -> Int -> Complex
toComplex a b = (Complex a b)  

-- prints a complex number in the form of 'a +/- bi'
complexToString :: Complex -> String
complexToString (Complex r i) | i >= 0 = show r ++ "+" ++ show i ++ "i"
						   | otherwise = show r ++ show i ++ "i"
			
-- calculates the absolute value of a single complex number			
complexAbs :: Complex -> Double
complexAbs (Complex a b) = sqrt $ fromIntegral $ a^2 + b^2  
complexAbs _ = error "invalid number!"

-- evaluates the argument phi of a complex number
getArgument :: Complex -> Double
getArgument (Complex a b)
	| abs == 0 = 0
	| b >= 0 = acos (fromIntegral b / abs)
	| otherwise = (-1) * (acos (fromIntegral b / abs))
	where abs = complexAbs (Complex a b)
	      arg = getArgument (Complex a b)
getArgument _ = error "invalid number!"

-- converts a complex number into the polar form and returns the sin and cos values
-- of it's parts
complexToPolar :: Complex -> (Double, Double, Double)
complexToPolar (Complex a b) = (abs, cos arg, sin arg)
					where arg = getArgument (Complex a b)
					      abs = complexAbs (Complex a b)
						  
-- converts a triple of absolute amount, sinus(phi) and cosinus(phi) to a complex number
polarToComplex :: (Double, Double, Double) -> Complex
polarToComplex (abs, cosPhi, sinPhi) = (Complex (round (abs * cosPhi)) (round (abs * sinPhi)))
						  
-- prints out the polar form of a complex number
printPolar :: (Double, Double, Double) -> String
printPolar (abs, cosPhi, sinPhi) = "(" ++ show abs ++ "," ++ show cosPhi ++ "," ++ show sinPhi ++ ")"
				  
-- conjugates a complex number by flipping the algebraic sign
complexConjugate :: Complex -> Complex
complexConjugate (Complex a b) = Complex a (-b)
						   
-- adds two complex numbers
complexAdd :: Complex -> Complex -> Complex
complexAdd (Complex a b) (Complex c d) = (Complex (a + c) (b + d))

-- subtracts two complex numbers
complexSub :: Complex -> Complex -> Complex
complexSub (Complex a b) (Complex c d) = (Complex (a - c) (b - d))

-- multiplies two complex numbers
complexMult :: Complex -> Complex -> Complex
complexMult (Complex a b) (Complex c d) = (Complex (a*c - b*d) (a*d + b*c))

-- divides two complex numbers
complexDiv :: Complex -> Complex -> Complex
complexDiv (Complex a b) (Complex c d) = (Complex (a*c + b*d `div` c^2 + d^2) (b*c - a*d `div` c^2 + d^2))



						   

