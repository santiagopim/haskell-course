
-- Question 1 Add the type signatures for the functions below and then
-- remove the comments and try to compile. (Use the types presented in
-- the lecture.)

f1 :: Floating a => a -> a -> a -> a
f1 x y z = x ** (y/z)

f2 :: Floating a => a -> a -> a -> a
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: Eq a => [a] -> [a] -> [a] -> Bool
f4 x y z = x == (y ++ z)

-- Question 2 Why should we define type signatures of functions? How
-- can they help you? How can they help others?
{- |
It is recomended to define signature previous to write the function,
it helps in the function writing, and reading.
-}

-- Question 3 Why should you define type signatures for variables? How
-- can they help you?
{- |
Declaring exactly the type that one variable is limited to.
-}

-- Question 4 Are there any functions in Haskell that let you
-- transform one type to the other? Try googling for the answer.
{- |
Yes, there are some functions that can modify the type of the
input, for example the function `toInteger' or the function `show'.
But the input type must be convertible.

toInteger :: Integral a => a -> Integer
show :: Show a => a -> String

-}

-- Question 5 Can you also define in Haskell list of lists? Did we
-- showed any example of that? How would you access the inner most
-- elements?
{- |
Yes, you can define lists of lists. Example:
-}

multilist :: Num a => [[a]]
multilist = [[1, 2, 3], [8, 9]]

getInner :: Int -> Int -> [[a]] -> a
getInner i j ll = (ll !! i) !! j

-- getInner 0 1 multilist
-- -> 2
