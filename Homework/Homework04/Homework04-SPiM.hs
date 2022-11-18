-- Question 1 Lets say you have the nested values defined bellow. How
-- would you get the value of 4 by using only pattern matching in a
-- function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

nv :: [([Int], [Int])] -> Int
nv [(_,[_,x]), _] = x
  
-- Question 2 Write a function that takes a list of elements of any
-- type and, if the list has 3 or more elements, it removes them.
-- Else, it does nothing. Do it two times, one with multiple function
-- definitions and one with case expressions.
dl1 :: [a] -> [a]
dl1 []       = []
dl1 (x:[])   = [x]
dl1 (x:y:zs) = (x:y:[])

dl2 :: [a] -> [a]
dl2 l = case l of
  []       -> []      
  (x:[])   -> [x]     
  (x:y:zs) -> (x:y:[])

-- Question 3 Create a function that takes a 3-element tuple (all of
-- type Integer) and adds them together
adder :: (Integer, Integer, Integer) -> Integer
adder (a, b, c) = a + b + c

-- Question 4 Implement a function that returns True if a list is
-- empty and False otherwise.
listChecker :: [a] -> Bool
listChecker [] = True
listChecker _  = False

-- Question 5 Write the implementation of the tail function using
-- pattern matching. But, instead of failing if the list is empty,
-- return an empty list.
tail2 :: [a] -> [a]
tail2 []     = []
tail2 (x:xs) = xs

-- Question 6 write a case expression wrapped in a function that takes
-- an Int and adds one if it's even. Otherwise does nothing. (Use the
-- `even` function to check if the number is even.)
add1Even :: Int -> Int
add1Even i = case (even i) of
  True  -> i + 1
  False -> i
  
