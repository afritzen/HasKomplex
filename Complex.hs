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

-- takes two numbers and wraps them to the complex type
toComplex :: Int -> Int -> Complex
toComplex a b = (Complex a b)  

-- prints a complex number in the form of 'a +/- bi'
complexToString :: Complex -> String
complexToString (Complex r i) | i >= 0 = show r ++ "+" ++ show i ++ "i"
						   | otherwise = show r ++ show i ++ "i"
			
-- calculates the absolute value of a single complex number			
complexAbs :: Complex -> Int
complexAbs (Complex a b) = round . sqrt $ fromIntegral $ a^2 + b^2  
complexAbs _ = error "invalid number!"

-- conjugates a complex number by flipping the algebraic sign
complexConjugate :: Complex -> Complex
complexConjugate (Complex a b) = (Complex a (-b))
complexConjugate _ = error "invalid number!"
						   
-- adds two complex numbers
complexAdd :: Complex -> Complex -> Complex
complexAdd (Complex a b) (Complex c d) = (Complex (a + c) (b + d))
complexAdd _ _ = error "invalid number(s)!"

-- subtracts two complex numbers
complexSub :: Complex -> Complex -> Complex
complexSub (Complex a b) (Complex c d) = (Complex (a - c) (b - d))
complexSub _ _ = error "invalid number(s)!" 

-- multiplies two complex numbers
complexMult :: Complex -> Complex -> Complex
complexMult (Complex a b) (Complex c d) = (Complex (a*c - b*d) (a*d + b*c))
complexMult _ _ = error "invalid number(s)!"

-- divides two complex numbers
complexDiv :: Complex -> Complex -> Complex
complexDiv (Complex a b) (Complex c d) = (Complex (a*c + b*d `div` c^2 + d^2) (b*c - a*d `div` c^2 + d^2))
complexDiv _ _ = error "invalid number(s)!"



						   

