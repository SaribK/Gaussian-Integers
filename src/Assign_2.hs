{- Assignment 2
 - Name: Sarib Kashif
 - Date: October 17 2019
 -}

module Assign_2 where

macid :: String
macid = "kashis2"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: 
 - returns the real part of GaussianInt
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x,_) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description: 
 - returns the imaginary part of Gaussian Int
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (_,y) = y

{- -----------------------------------------------------------------
 - gausConj
 - -----------------------------------------------------------------
 - Description: 
 - returns g but the imaginary part is negated
 - imaginary and real parts are obtained from previous 2 functions
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj g = (gaussReal g, -gaussImag g)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: 
 - adds the real parts of g0 and g1 to return the real part of the output
 - adds the imaginary parts of g0 and g1 to return the imaginary part of the output
 - real and imaginary parts are obtained from the first 2 functions
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = (gaussReal g0 + gaussReal g1, gaussImag g0 + gaussImag g1)

{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: 
 - function defines x0, x1, y0, y1 as g0 = (x0,y0) and g1 = (x1,y1)
 - 
 -}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult (x0,y0) (x1,y1) = (x2,y2) 
    where 
        x2 = x0 * x1 - y0 * y1
        y2 = x0 * y1 + y0 * x1 
            
{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: 
 - multiplies the gaussianInt that is inputted and its conjugate
 - adds the imaginary and real parts of the multiplied gaussianInt
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm (x,y) = x^2 + y^2

{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: 
 - uses pattern matching.
 - either has empty list or not empty list (which uses recursion)
 - simplest case is when the list has one value in it.
 - if there's more than one value, then finds the gaussNorm
   of the first 2 values and compares them.
 - if first value is greater than equal to second value, function calls
   itself again but removes second value.
 - if second value is greater than first, function calls itself but removes
   first value.
 -}
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm [] = (0,0)
maxGaussNorm (x:xs) 
    | xs == [] = x
    | g1 >= g2 = maxGaussNorm (x:tail xs) 
    | g1 < g2 = maxGaussNorm xs
    where
        g1 = gaussNorm x
        g2 = gaussNorm (head xs)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: (5, 1)
 - - Expected Output: (5, -1)
 - - Acutal Output: (5, -1)

 - - Function: gaussConj
 - - Test Case Number: 2
 - - Input: (-1, -1)
 - - Expected Output: (-1, 1)
 - - Acutal Output: (-1, 1)

 - - Function: gaussConj
 - - Test Case Number: 3
 - - Input: (0, 0)
 - - Expected Output: (0, 0)
 - - Acutal Output: (0, 0)

 - - Function: gaussAdd
 - - Test Case Number: 1
 - - Input: (2,2) (0,0)
 - - Expected Output: (2,2)
 - - Acutal Output: (2,2)

 - - Function: gaussAdd
 - - Test Case Number: 2
 - - Input: (-5,1) (3,0)
 - - Expected Output: (-2,1)
 - - Acutal Output: (-2,1)

 - - Function: gaussAdd
 - - Test Case Number: 3
 - - Input: (1,0) (0,1)
 - - Expected Output: (1,1)
 - - Acutal Output: (1,1)

 - - Function: gaussMult
 - - Test Case Number: 1
 - - Input: (0,0) (5,7)
 - - Expected Output: (0,0) 
 - - Acutal Output: (0,0)

 - - Function: gaussMult
 - - Test Case Number: 2
 - - Input: (2,4) (-1,5)
 - - Expected Output: (-22,6)
 - - Acutal Output: (-22,6)

 - - Function: gaussMult
 - - Test Case Number: 3
 - - Input: (1,3) (0, -1) 
 - - Expected Output: (3,-1)
 - - Acutal Output: (3,-1)

 - - Function: gaussNorm
 - - Test Case Number: 1
 - - Input: (3,2)
 - - Expected Output: 13
 - - Acutal Output: 13

 - - Function: gaussNorm
 - - Test Case Number: 2
 - - Input: (-5,5)
 - - Expected Output: 50
 - - Acutal Output: 50

 - - Function: gaussNorm
 - - Test Case Number: 3
 - - Input: (0,0)
 - - Expected Output: 0
 - - Acutal Output: 0

 - - Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input: []
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)

 - - Function: maxGaussNorm
 - - Test Case Number: 2
 - - Input: [(0,0),(1,2),(5,4)]
 - - Expected Output: (5,4)
 - - Acutal Output: (5,4)

 - - Function: maxGaussNorm
 - - Test Case Number: 3
 - - Input: [(1,-5),(3,2),(0,4)]
 - - Expected Output: (1,-5)
 - - Acutal Output: (1,-5)
 -}

