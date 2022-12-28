import qualified System.Random   as R
import qualified Test.QuickCheck as Q

-- Question 1 --

-- Continuing with the logistics software of the lesson:

-- 1. After using the `Container` type class for a while, you realize
-- that it might need a few adjustments:

-- - First, write down the `Container` type class and its instances,
-- same as we did in the lesson (try to do it without looking and
-- check at the end or if you get stuck).

-- - Then, add a function called `unwrap` that gives you back the
-- value inside a container.

data Box a = EmptyBox
           | Has a
           deriving (Show)

data Present t p = EmptyPresent t
                 | PresentFor t p
                 deriving (Show)

class Container c where
  isEmpty  :: c a -> Bool
  contains :: (Eq a) => c a -> a -> Bool
  replace  :: c a -> b -> c b
  unwrap   :: c a -> Maybe a
  
instance Container Box where
  isEmpty EmptyBox = True
  isEmpty _        = False

  contains EmptyBox _  = False
  contains (Has x)  y  = x == y

  replace _ x = Has x

  unwrap EmptyBox = Nothing
  unwrap (Has x)  = Just x

instance Container (Present t) where
  isEmpty (EmptyPresent _) = True
  isEmpty _                = False

  contains (EmptyPresent _) _ = False
  contains (PresentFor _ x) y = x == y

  replace (EmptyPresent t) x = PresentFor t x
  replace (PresentFor t _) x = PresentFor t x

  unwrap (EmptyPresent _) = Nothing
  unwrap (PresentFor _ x) = Just x

-- 2. Create an instance for the `MailedBox` data type.
-- - The MailedBox data type represents a box sent through the mail.
-- - The parameter `t` is a tag with a person's identifier
-- - The parameter `d` is the person's details (address,etc).
-- - The parameter `a` is the content of the MailedBox

data MailedBox t d a = EmptyMailBox t d
                     | MailBoxTo t d a

instance Container (MailedBox t d) where
  isEmpty (EmptyMailBox _ _) = True
  isEmpty _                  = False

  contains (EmptyMailBox _ _) _ = False
  contains (MailBoxTo _ _ x)  y = x == y

  replace (EmptyMailBox t d) x = MailBoxTo t d x
  replace (MailBoxTo t d _)  x = MailBoxTo t d x

  unwrap (EmptyMailBox _ _) = Nothing
  unwrap (MailBoxTo _ _ x)  = Just x

-- Question 2 --

-- Create instances for Show, Eq, and Ord for these three data types
-- (use automatic deriving whenever possible):

data Position = Intern
              | Junior
              | Senior
              | Manager
              | Chief
              deriving (Eq, Show)

instance Ord Position where
  compare Intern Junior  = LT
  compare Junior Senior  = LT
  compare Senior Manager = LT
  compare Manager Chief  = LT

data Experience = Programming
                | Managing
                | Leading
                deriving (Eq, Show)

instance Ord Experience where
  compare Programming Managing = LT
  compare Managing Leading     = LT

type Address = String

data Salary = USD Double
            | EUR Double
            deriving (Eq, Show, Ord)

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address
  deriving (Eq, Show)

instance Ord Relationship where
  compare (Employee _ _ _ _)    (Contractor _ _ _ _)  = LT
  compare (Employee _ _ s1 _)   (Employee _ _ s2 _)   = compare s1 s2
  compare (Contractor p1 _ _ _) (Contractor p2 _ _ _) = compare p1 p2

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  } deriving (Eq, Show)
             

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

instance Ord Pokemon where
  compare (Pokemon _ _ g1 x1) (Pokemon _ _ g2 x2) = compare
                                                    (g1 * getRandom x2)
                                                    (g2 * getRandom x2)

getRandom :: Int -> Int
getRandom s = do
  let (r, g) = R.random (R.mkStdGen s)
  r

-- Question 3 -- EXTRA CREDITS

-- Uncomment the next code and make it work (Google what you don't
-- know).

-- Team memeber experience in years
newtype Exp = Exp Double deriving (Show)

-- Team memeber data
type TeamMember = (String, Exp)

-- List of memeber of the team
team :: [TeamMember]
team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]

-- Function to check the combined experience of the team
-- This function applied to `team` using GHCi should work
combineExp :: [TeamMember] -> Exp
combineExp = foldr ((+) . snd) 0

-- The error is:
-- Homework-SPiM.hs:162:21-23: error: …
--     • No instance for (Num Exp) arising from a use of ‘+’
--     • In the first argument of ‘(.)’, namely ‘(+)’
--       In the first argument of ‘foldr’, namely ‘((+) . snd)’
--       In the expression: foldr ((+) . snd) 0
--     |
-- Compilation failed.

instance Num Exp where
  (+) (Exp x) (Exp y) = Exp (x + y)
  (*) (Exp x) (Exp y) = Exp (x * y)
  abs (Exp x)         = Exp (abs x)
  signum (Exp x)      = Exp (signum x)
  fromInteger x       = Exp (fromInteger x)
  negate (Exp x)      = Exp (negate x)

-- Also adding the `deriving (Show)` to the Exp definition.
