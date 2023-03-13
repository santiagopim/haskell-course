{-# LANGUAGE OverloadedStrings #-}

import qualified System.Random as R

data Pokemon = Pokemon
  { pName :: String,
    pType :: [String],
    pGeneration :: Int,
    pPokeDexNum :: Int,
    pPower :: Int
  }
  deriving (Show, Eq)

charizard :: Pokemon
charizard = Pokemon "Charizard" ["Fire", "Flying"] 1 6 20

venusaur :: Pokemon
venusaur = Pokemon "Venusaur" ["Grass", "Poison"] 1 3 18

battle :: Pokemon -> Pokemon -> IO ()
battle p1 p2 = do
  r1 <- R.randomIO :: IO Int
  r2 <- R.randomIO :: IO Int
  if pPower p1 * r1 > pPower p2 * r2
    then putStrLn ((pName p1) ++ " wins with " ++ (show ((pPower p1) * r1)) ++ " over " ++ (show ((pPower p2) * r2)))
    else putStrLn ((pName p2) ++ " wins with " ++ (show ((pPower p2) * r2)) ++ " over " ++ (show ((pPower p1) * r1)))
