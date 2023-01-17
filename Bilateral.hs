-- any imports go here
import Data.List
{-Begin Question 2.1-}

-- number takes a list of digits and returns the positive integer formed from those digits
number :: [Int] -> Int
-- Use foldl to iterate over the list of digits and build the resulting integer.
-- The lambda function takes the current result and the next digit from the list,
-- and returns a new result by appending the next digit to the end of the current result.
number = foldl (\x y -> 10 * x + y) 0  
{-End Question 2.1-}

{-Begin Question 2.2-}
-- The splits function takes a list and returns all of the non-trivial splits of that list as a list of pairs
splits :: [a] -> [([a], [a])]
-- Use a list comprehension to generate all possible non-trivial splits of the input list.
-- For each split, create a tuple containing the elements of the input list that appear before and after the split point.
splits xs = [(take i xs, drop i xs) | i <- [1..length xs - 1]]

-- The possibles function returns all 2,903,040 pairs that make up the Teaser solution space
possibles :: [([Int], [Int])]
-- Use concatMap to apply the splits function to each permutation of the list [1,2,3,4,5,6,7,8,9].
-- This generates a list of all possible splits of each permutation, and concatenates these lists into a single list of all possible splits of any permutation.
possibles = concatMap splits (permutations [1,2,3,4,5,6,7,8,9])

{-End Question 2.2-}

{-Begin Question 2.3-}
-- isAcceptable takes a pair of lists of digits and returns true if the pair is an acceptable solution to the Teaser, and false otherwise
-- The isAcceptable function takes a pair of lists of integers as its argument and returns a boolean value indicating whether or not the pair is acceptable.
isAcceptable :: ([Int], [Int]) -> Bool
isAcceptable (xs, ys) = 
    -- First, calculate the product of the numbers represented by the two lists.
    -- Use the number function to convert the lists of integers into single numbers.
    let product = number xs * number ys
    -- Check if the product is a palindrome, which means that its digits are the same forwards and backwards.
    -- Do this by checking if the string representation of the product is the same as its reverse.
    in (show product == reverse (show product))
    -- Check if the first digit of the product is '4'.
    -- Do this by taking the string representation of the product and checking the first character of the string.
    && (head (show product) == '4')
    -- Check if the last digit of the smaller of the two numbers is '3'.
    -- Do this by taking the string representation of the minimum of the two numbers and checking the last character of the string.
    && (last (show (minimum [number xs, number ys])) == '3')

-- The acceptables function returns all pairs from the Teaser solution space that are considered acceptable according to the isAcceptable function.
acceptables :: [([Int], [Int])]
-- Use filter to apply the isAcceptable function to each pair in the possibles list.
-- This generates a list of all pairs that are considered acceptable according to the isAcceptable function.
acceptables = filter isAcceptable possibles

{-End Question 2.3-}

-- any main functions for testing goes here
main :: IO ()
main = do
    putStrLn "Testing question 2.1:"
    -- Test the number function with various input values
    putStrLn "\nTesting number function with [9,1,2,4] :"
    let result1 = number [9,1,2,4]
    putStrLn "Should print 9124"
    putStrLn (show result1) -- Should print 9124

    putStrLn "\nTesting number function with [1,0,0,1] :"
    let result2 = number [1,0,0,1]
    putStrLn "Should print 1001"
    putStrLn (show result2) -- Should print 1001

    putStrLn "\nTesting number function with [0,0,0,1] :"
    let result3 = number [0,0,0,1]
    putStrLn "Should print 1"
    putStrLn (show result3) -- Should print 1
    
    putStrLn "------------------------------------------------------------"

    putStrLn "\n Testing question 2.2:"
    -- Test the splits function with various input values
    putStrLn "\nTesting splits function with [1,2,3,4] :"
    let result4 = splits [1,2,3,4]
    putStrLn "Should print [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]"
    putStrLn (show result4) -- Should print [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
    putStrLn "\nTesting splits function with [1,2] :"
    let result5 = splits [1,2]
    putStrLn "Should print [([1],[2])]"
    putStrLn (show result5) -- Should print [[1],[2]]

    putStrLn "\nTesting splits function with [9,1,2,4,5] :"
    let result5 = splits [9,1,2,4,5]
    putStrLn "Should print [([9],[1,2,4,5]),([9,1],[2,4,5]),([9,1,2],[4,5]),([9,1,2,4],[5])]"
    putStrLn (show result5) -- [([9],[1,2,4,5]),([9,1],[2,4,5]),([9,1,2],[4,5]),([9,1,2,4],[5])]

    -- Test the possibles function
    putStrLn "\nTesting possibles function :"
    let result6 = possibles
    putStrLn "Should print the first few pairs in the list of possibles"
    putStrLn (show (take 10 result6)) -- Should print the first 10 pairs in the list

    putStrLn "\nTesting number of possibles :"
    let result7 = length possibles
    putStrLn "Should print the number of possibles, which is 2,903,040:"
    putStrLn (show result7) -- Should print 2903040
    
    putStrLn "------------------------------------------------------------"

    putStrLn "Testing question 2.3:"
    -- Test the isAcceptable function with various input value
    putStrLn "\nTesting isAcceptable function with ([7,1,6,3], [5,9,2,4,8]) :"
    let result8 = isAcceptable ([7,1,6,3], [5,9,2,4,8])
    putStrLn "Should print True"
    putStrLn (show result8) -- Should print True

    putStrLn "\nTesting isAcceptable function with ([1,2,3,4], [5,6,7,8,9]) :"
    let result9 = isAcceptable ([1,2,3,4], [5,6,7,8,9])
    putStrLn "Should print False"
    putStrLn (show result9) -- Should print False

    putStrLn "\nTesting isAcceptable function with ([2,9,6,7,8],[1,4,5,3]) :"
    let result10 = isAcceptable ([2,9,6,7,8],[1,4,5,3])
    putStrLn "Should print True"
    putStrLn (show result10) -- Should print True

    putStrLn "\nTesting isAcceptable function with ([3,2,1,7,5,6,3], [8,9,2,4]) :"
    let result11 = isAcceptable ([3,2,1,7,5,6,3], [8,9,2,4])
    putStrLn "Should print False"
    putStrLn (show result11) -- Should print False

    -- Test the acceptables function
    putStrLn "\nTesting acceptables function :"
    let result12 = acceptables
    putStrLn "Should print the 6 pairs in the list of acceptables"
    putStrLn (show result12) -- Should print the 6 pairs in the list

    putStrLn "\nTesting number of acceptables :"
    let result13 = length acceptables
    putStrLn "Should print the number of acceptables, which is 6:"
    putStrLn (show result13) -- Should print 6