import Debug.Trace

{- |
Trace example. Can use the guard or the inline ones. the trace
function prints the first parameter and executes/returns the second.
Needs to import the Debug.Trace module.
-}
and' :: [Bool] -> Bool
and' []     | trace ("In guard: []")              False = undefined
and' (x:xs) | trace ("In guard: " ++ show (x:xs)) False = undefined
and' []     = trace ("In execution line: []")              $ True
and' (x:xs) = trace ("In execution line: " ++ show (x:xs)) $ x && and' xs

-- Question 1 Write a function that checks if the monthly consumption
-- of an electrical device is bigger, equal, or smaller than the
-- maximum allowed and returns a message accordingly. The function has
-- to take the hourly consumption of an electrical device, the hours
-- of daily use, and the maximum monthly consumption allowed. (Monthly
-- usage = consumption (kW) * hours of daily use (h) * 30 days).
data Consumption = Bigger | Equal | Smaller
  deriving Show

checkMonthConsumption :: Int -> Int -> Int -> Consumption
checkMonthConsumption c h m
  | mc < m  = Smaller
  | mc == m = Equal
  | mc > m  = Bigger
  where mc = c * h * 30

-- Question 2 Prelude: We use the function `show :: a -> String` to
-- transform any type into a String. So `show 3` will produce `"3"`
-- and `show (3 > 2)` will produce `"True"`.
-- In the previous function, return the excess/savings of consumption
-- as part of the message.
checkMonthConsumption2 :: Int -> Int -> Int -> String
checkMonthConsumption2 c h m
  | dc < 0  = "The consumption is smaller by " ++ show dc
  | dc == 0 = "The consumption is equal to the allowed one"
  | dc > 0  = "The consumption is bigger by " ++ show dc
  where dc = m - (c * h * 30)

-- Question 3 Write a function that showcases the advantages of using
-- let expressions to split a big expression into smaller ones. Then,
-- share it with other students in Canvas.
{- |
The quadratic formula solves the quadratic equation:

a·x^2 + b·x + c = 0

With a ≠ 0, the quadratic formula is:

x = (-b ± sqrt( b^2 - 4·a·c)) / (2·a)

Samples:
λ> quadratic (1/2) (-5/2) 2
(4.0,1.0)
λ> quadratic 1 2 1
(-1.0,-1.0)
λ> quadratic 2 3 4
(NaN,NaN)
-}
quadratic :: Double -> Double -> Double -> (Double, Double)
quadratic a b c = let d  = b * b - 4 * a * c
                      rd = sqrt d
                      s1 = (-b + rd) / (2 * a)
                      s2 = (-b - rd) / (2 * a)
                  in (s1, s2)

-- Question 4 Write a function that takes in two numbers and returns
-- their quotient such that it is not greater than 1. Return the
-- number as a string, and in case the divisor is 0, return a message
-- why the division is not possible. To implement this function using
-- both guards and if-then-else statements.
-- abs :: Double -> Double
-- abs n | n >= 0    = n
--       | otherwise = (-n)
quotient :: Double -> Double -> String
quotient x y
  | x == 0 && y == 0 = "Indetermination 0/0"
  | x == 0 || y == 0 = "0"
  | otherwise        = if abs (x / y) <= 1 
                       then show (x / y)
                       else show (y / x)

-- Proposed solution fails in numerator 0
guardsAndIf :: Double -> Double -> String
guardsAndIf a b
  | a > b = if a /= 0 then show (a/b) else "a is larger but 0"
  | a < b = if b /= 0 then show (b/a) else "b is larger but 0"
  | otherwise = if a /= 0 then "1" else "a and b are both 0"

-- Sara Tomaz solution, fails in negative numbers
makeDivision :: Double -> Double -> String
makeDivision x y
  | x == 0 && y == 0 = "impossible to divide by zero"
  | x == y = "quotient is 1"
  | otherwise = if x > y then show (y / x) else show (x / y)

-- Question 5 Write a function that takes in two numbers and
-- calculates the sum of squares for the product and quotient of those
-- numbers. Write the function such that you use a where block inside
-- a let expression and a let expression inside a where block.
rareSum :: Double -> Double -> Double
rareSum x y = 
  let qq = q * q where q = x / y
  in pp + qq
  where pp = let p = x * y
             in p * p
