{-# LANGUAGE NamedFieldPuns #-}
{-
**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a
similar but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the
player is now in a forest. Because we're in a forest, there are no
walls. And, if you walk long enough, you're guaranteed to find the
exit.

So, what's the challenge in playing this game? The challenge lies in
that now we have "stamina." Stamina is a number (we start with 10).
And, each time the player makes a move, its stamina gets reduced by
the amount of work needed to cross the current trail (represented by a
number contained in the value constructor).

The data types and functions are pretty much the same, with a few
caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make
sure we can do basic numeric operations regardless of the type we pass
to the functions.
- Because now we have to keep track of the player's stamina, we'll
need to move it around with our current forest. This would be an
awesome use case for monads, but because we don't know how to use them
yet, a "(stamina, forest)" pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

-- So there are multiple paths to get the exit. Consider the forest as
-- a grid, with the starting point, the starting direction and the
-- exit position relative to the starting position and direction.

data Move = GoForward
          | GoLeft
          | GoRight
          deriving (Show)

type Position = (Int, Int)
type Stamina = Int 
data Status = Exit
            | NoStamina
            | InForest Position Stamina
            deriving (Show)

-- The movement has two parts, consuming the stamina and translating
-- the player. If the stamina is depleted then the status is
-- NoStamina.

consumeStamina :: Status -> Stamina -> Status
consumeStamina (InForest (x, y) s) consume
  | (s - consume) <= 0 = NoStamina
  | otherwise          = InForest (x, y) (s - consume)

applyMove :: Status -> Move -> Status
applyMove NoStamina           _         = NoStamina
applyMove (InForest (x, y) s) GoForward = InForest (x - 1   , y) s
applyMove (InForest (x, y) s) GoLeft    = InForest ((y - 1) , (-x)) s
applyMove (InForest (x, y) s) GoRight   = InForest (-(y + 1), x) s

move :: Status -> Move -> Status
move s m = applyMove (consumeStamina s 3) m

-- Apply the movements list recursively, then transform to appropiated
-- string depending on final status

solve :: Status -> [Move] -> Status
solve NoStamina           _      = NoStamina
solve (InForest (0, 0) _) _      = Exit
solve (InForest (x, y) s) []     = InForest (x, y) s
solve (InForest (x, y) s) (m:ms) = solve (move (InForest (x, y) s) m) ms

showCurrentChoice :: Status -> String
showCurrentChoice Exit           = "YOU'VE FOUND THE EXIT !!"
showCurrentChoice NoStamina      = "You ran out of stamina and died -.-!"
showCurrentChoice (InForest _ s) = "You have " ++ show s ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

solveForest :: Status -> [Move] -> String
solveForest st mo = showCurrentChoice $ solve st mo

-- The testing case:

testForest = InForest (2, 1) 10

-- λ> solveForest testForest []
-- "You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
-- λ> solveForest testForest [GoForward]
-- "You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
-- λ> solveForest testForest [GoForward, GoForward]
-- "You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
-- λ> solveForest testForest [GoForward, GoForward, GoRight]
-- "You have 1 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
-- λ> solveForest testForest [GoForward, GoForward, GoRight, GoForward]
-- "You ran out of stamina and died -.-!"
-- λ> solveForest testForest [GoForward, GoForward, GoLeft]
-- "YOU'VE FOUND THE EXIT !!"

-- Trace the path to debug -----------------------------------------------------
tracePath :: Status -> [Move] -> [Status]
tracePath _                   []     = []
tracePath Exit                _      = []
tracePath NoStamina           _      = []
tracePath (InForest (x, y) s) (m:ms) = (move (InForest (x, y) s) m) : tracePath (move (InForest (x, y) s) m) ms

-- λ> tracePath InForest {exitAt = (3, 3), stamina = 3} [GoForward, GoForward, GoLeft, GoLeft, GoForward, GoForward, GoRight, GoRight, GoForward, GoForward, GoForward, GoLeft]
-- [InForest {exitAt = (2,3), stamina = 2},InForest {exitAt = (1,3), stamina = 1},InForest {exitAt = (2,-1), stamina = 0},NoStamina]
