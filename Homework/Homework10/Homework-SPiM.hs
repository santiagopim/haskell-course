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

  unwrap (EmptyPresent t) = Nothing
  unwrap (PresentFor t x) = Just x


-- 2. Create an instance for the `MailedBox` data type.
-- - The MailedBox data type represents a box sent through the mail.
-- - The parameter `t` is a tag with a person's identifier
-- - The parameter `d` is the person's details (address,etc).
-- - The parameter `a` is the content of the MailedBox

data MailedBox t d a = EmptyMailBox t d | MailBoxTo t d a

-- Question 2 --

-- Create instances for Show, Eq, and Ord for these three data types
-- (use automatic deriving whenever possible):

data Position = Intern | Junior | Senior | Manager | Chief

data Experience = Programming | Managing | Leading

type Address = String

data Salary = USD Double | EUR Double

data Relationship
  = Contractor Position Experience Salary Address
  | Employee Position Experience Salary Address

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int
  }

charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6

venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3

-- Question 3 -- EXTRA CREDITS

-- Uncomment the next code and make it work (Google what you don't
-- know).

-- -- Team memeber experience in years
-- newtype Exp = Exp Double
--
-- -- Team memeber data
-- type TeamMember = (String, Exp)
--
-- -- List of memeber of the team
-- team :: [TeamMember]
-- team = [("John", Exp 5), ("Rick", Exp 2), ("Mary", Exp 6)]
--
-- -- Function to check the combined experience of the team
-- -- This function applied to `team` using GHCi should work
-- combineExp :: [TeamMember] -> Exp
-- combineExp = foldr ((+) . snd) 0
