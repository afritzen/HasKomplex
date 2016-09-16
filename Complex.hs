{-- A module for complex numbers providing all common operations. --}

module Complex where

data Complex = Complex Int Int
			   deriving (Show, Eq, Ord, Read)

-- complex number with positive real and negative imaginary part 
complex1 :: Complex
complex1 = Complex 2 (-3)

-- complex number with negative real and positive imaginary part 
complex2 :: Complex
complex2 = Complex (-4) 2 

-- prints a complex number in the form of 'a +/- bi'
complexToString :: Complex -> String
complexToString (Complex r i) | i >= 0 = show r ++ "+" ++ show i ++ "i"
						   | otherwise = show r ++ show i ++ "i"
						   

