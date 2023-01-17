-- any imports go here
import Data.List
{-Begin Question 1.1-}
-- This function takes a positive integer and returns a list of its digits that are greater than 0.
-- If the input value is not a positive integer, the function returns an empty list.
digits :: Int -> [Int]
digits n
  | n > 0 = map (\x -> read [x] :: Int) (show n) -- If the input integer is positive, use map and the show function to convert it to a string and then convert each character of the string to an integer.
  | otherwise = [] -- If the input integer is negative or zero, return an empty list.
{-End Question 1.1-}

{-Begin Question 1.2-}
-- Returns True if the given integer is a PAR, and False otherwise.
-- isPar checks if a given integer is a PAR by checking the following conditions:
-- 1. The number has 4 digits
-- 2. The number does not contain 0
-- 3. The last 2 digits of the number are a multiple of the first 2 digits
-- 4. The 4 digits of the number are all distinct
isPar :: Int -> Bool
isPar x =
  let ds = digits x in
  -- check if number has 4 digits
  length ds == 4 &&
  -- check if number does not contain 0
  not (0 `elem` ds) &&
  let a = ds !! 0 in
  let b = ds !! 1 in
  let c = ds !! 2 in
  let d = ds !! 3 in
  -- check if last 2 digits are a multiple of first 2 digits
  (10 * c + d) `mod` (10 * a + b) == 0 &&
  -- check if all digits are distinct
  a /= b && a /= c && a /= d &&
  b /= c && b /= d &&
  c /= d
-- pars is a list of all PARs that can be found by filtering all 4-digit numbers using the isPar function
pars :: [Int]
pars = filter isPar [1000..9999]

{-End Question 1.2-}

{-Begin Question 1.3-}
-- Returns True if the given pair of integers is a PARTY, and False otherwise
isParty :: (Int,Int) -> Bool
isParty (x,y) =
  let xs = digits x in
  let ys = digits y in
  -- Check that both numbers have 4 digits
  length xs == 4 && length ys == 4 &&
  -- Check that both numbers do not contain 0
  not (0 `elem` xs) && not (0 `elem` ys) &&
  -- Check that all digits in both numbers are distinct
  length (nub (xs ++ ys)) == 8 &&
  -- Check that there are no repeating digits in both numbers
  length (xs `intersect` ys) == 0 &&
  -- Check that there is exactly one missing digit in both numbers
  let ms = [1..9] \\ (xs ++ ys) in
  length ms == 1 &&
  -- Check that both numbers are multiples of the missing digit
  let m = head ms in
  (x `mod` m == 0) && (y `mod` m == 0)
-- List of all 14 PARTYs
partys :: [(Int,Int)]
partys = filter isParty [(x,y) | x <- pars, y <- pars, x /= y]

{-End Question 1.3-}

-- any main functions for testing goes here
main :: IO ()
main = do
  putStrLn "Testing question 1.1:"
  -- Test the digits function with various input values
  putStrLn "\nTesting digits function with 9124 :"
  let result1 = digits 9124
  putStrLn "Should print [9,1,2,4]"
  putStrLn (show result1) -- Should print [9,1,2,4]

  putStrLn "\nTesting digits function with 1001 :"
  let result3 = digits 1001
  putStrLn "Should print [1,0,0,1]"
  putStrLn (show result3) -- Should print [1,0,0,1]

  putStrLn "\nTesting digits function with 00001 :"  
  let result4 = digits 00001
  putStrLn "Should print [1]"
  putStrLn (show result4) -- Should print [1]

  putStrLn "\nTesting digits function with 98765 :"
  let result5 = digits 98765
  putStrLn "Should print [9,8,7,6,5]"
  putStrLn (show result5) -- Should print [9,8,7,6,5]

  putStrLn "\nTesting digits function with 0 :"
  let result6 = digits 0
  putStrLn "Should print []."
  putStrLn (show result6) -- Should print an empty list : []"

  putStrLn "\nTesting digits function with -1234 :"
  let result7 = digits (-1234)
  putStrLn "Should print []."
  putStrLn (show result7) -- Should print an empty list : []"
  
  putStrLn "------------------------------------------------------------"
  
  putStrLn "\n Testing question 1.2:"
  -- Test isPar function with different input values
  putStrLn "\nTesting isPar function with 2678 :"
  let result8 = isPar 2678
  putStrLn "Should print True"
  putStrLn (show result8) -- Should print True

  putStrLn "\nTesting isPar function with 2234 :"
  let result9 = isPar 2234
  putStrLn "Should print False"
  putStrLn (show result9) -- Should print False

  putStrLn "\nTesting isPar function with 1234 :"
  let result10 = isPar 1234
  putStrLn "Should print False"
  putStrLn (show result9) -- Should print False

  putStrLn "\nTesting isPar function with 345 :"
  let result11 = isPar 345
  putStrLn "Should print False"
  putStrLn (show result11) -- Should print False

  putStrLn "\nTesting isPar function with 1836 :"
  let result11 = isPar 1836
  putStrLn "Should print True"
  putStrLn (show result11) -- Should print True
  
  -- Test pars function
  putStrLn "\nTesting pars function :"
  let result12 = pars
  putStrLn "Should print all 44 PARs"
  putStrLn (show result12)

  putStrLn "\nTesting number of PARs :"
  let result18 = length pars
  putStrLn "Should print the number of PARs, which is 44:"
  putStrLn (show result18)
  
  putStrLn "------------------------------------------------------------"
  
  putStrLn "\n Testing question 1.3:"
  -- Test isParty function
  putStrLn "\nTesting isParty function with (2754, 1836) :"
  let result13 = isParty (2754, 1836)
  putStrLn "Should print True"
  putStrLn (show result13)

  putStrLn "\nTesting isParty function with (2678, 1234) :"
  let result14 = isParty (2678, 1234)
  putStrLn "Should print False"
  putStrLn (show result14)

  putStrLn "\nTesting isParty function with (2678, 12344) :"
  let result15 = isParty (2678, 12344)
  putStrLn "Should print False"
  putStrLn (show result15)

  -- Test partys function
  putStrLn "\nTesting partys function :"
  let result16 = partys
  putStrLn "Should print all 14 PARTYs"
  putStrLn (show result16)

  putStrLn "\nTesting number of PARTYs :"
  let result17 = length partys
  putStrLn "Should print the number of PARTYs, which is 14:"
  putStrLn (show result17)