
-- Question 1
-- Write a multiline comment below.
{- |
This is my multiline
comment.
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
multBy3 :: Num a => a -> a
multBy3 = (*) 3

-- Question 3
-- Define a function that calculates the area of a circle.
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

-- Question 4 Define a function that calculates the volume of a
-- cylinder by composing the previous function together with the
-- height of the cylinder.
cylinderVolume :: Floating a => a -> a -> a
cylinderVolume r h = circleArea r * h

-- Question 5 Define a function that takes the height and radius of a
-- cylinder and checks if the volume is greater than or equal to 42.
cylinderVolumeGTE42 :: (Ord a, Floating a) => a -> a -> Bool
cylinderVolumeGTE42 r h = (cylinderVolume r h) >= 42
