-- This homework is around creating Haskell types that represent wines
-- from over the world.

-- Question 1 Different wines are made from different grapes, there
-- are around 10000 varieties over the world! Create a type synonym
-- called "Grape" for the different grape names as strings.
-- Additionally, use this type synonym for the grapes: "Sangiovese",
-- "Cabernet-sauvignon", "Merlot" and "Garnacha".

type Grape = String

sangiovese        = "Sangiovese" :: Grape
cabernetSauvignon = "Cabernet-sauvignon" :: Grape
merlot            = "Merlot" :: Grape
garnacha          = "Garnacha" :: Grape

-- Question 2 The most famous regions that export wine are located in
-- France, Italy and Spain. Each of these countries is divided up in
-- smaller regions. These smaller regions are known for a certain
-- style, for example the Champagne region in France Create a type
-- synonym called "Region" for wine region given their country and
-- region as a tuple of strings. Additionally, use this type synonym
-- for the regions: Bordeaux in France, Tuscany in Italy and Rioja in
-- Spain.

type Region = (String, String)

bordeaux = ("France", "Bordeaux") :: Region
tuscany  = ("Italy", "Tuscany") :: Region
rioja    = ("Spain", "Rioja") :: Region

-- Question 3 A wine is either one of three kinds, these are red,
-- white or rose wine. Besides its kind, each wine also has a given
-- alcohol level. Create a data type called "Kind" that represents
-- these three kinds, with each capturing the level of alcohol.
-- Additionally, use this data type for the examples: red wine with
-- 14.5% alcohol, white wine with 13% alcohol and Rose wine with 12%
-- alcohol.

data Kind = Red Float
          | White Float
          | Rose Float

w1 = Red 14.5 :: Kind
w2 = White 13 :: Kind
w3 = Rose 12 :: Kind

-- Question 4 In the world of wines, bottles display all of the above
-- information for the consumer on its label. Create a record type
-- called "Label" that captures the grapes that are in a whine, the
-- region its from, and it's kind. Notice that some wines are a
-- blended combination of multiple grapes! Additionally, create for
-- each of the described wine below a label.

data Label = Label { grapes :: [Grape]
                   , region :: Region
                   , kind :: Kind
                   }

-- Larrosa Rose is a rose wine from the region Rioja. It is made from
-- the Garnacha grape and has a alcohol level of 14%.

larrosaRose = Label { grapes = [garnacha]
                    , region = tuscany
                    , kind = Rose 14
                    }

-- Castiglioni is a red wine from the region of Tuscany. It is made
-- from the grape Sangiovese and has an alcohol level of 12.5%.

castiglioni = Label { grapes = [sangiovese]
                    , region = tuscany
                    , kind = Red 12.5
                    }

-- Bordeaux is known for its red wine, these are mainly a blend
-- between Cabernet-sauvignon and Merlot. Create a Label for the wine
-- "Le Petit Haut Lafitte" that has an alcohol percentage 13.5%.

lePetitHautLafitte = Label { grapes = [cabernetSauvignon, merlot]
                           , region = bordeaux
                           , kind = Red 13.5
                           }

-- Question 5 Write a function `containsGrape` that takes a list of
-- Labels and a Grape and returns a boolean. The function should check
-- if the there exists a wine in the Label that contains this Grape.

containsGrape :: [Label] -> Grape -> Bool
containsGrape []     _ = False
containsGrape (l:ls) g = checkGrape (grapes l) g || containsGrape ls g

checkGrape :: [Grape] -> Grape -> Bool
checkGrape []     _     = False
checkGrape (g:gs) grape = (g == grape) || checkGrape gs grape

-- The solution uses lambda and elem functions:

containsGrape' :: [Label] -> Grape -> Bool
containsGrape' ls g = any (\Label{grapes=gs} -> elem g gs) ls

-- This is a test list for the `containsGrape` function with an grape
-- that is not in the list.
grapeList = [larrosaRose, castiglioni, lePetitHautLafitte] :: [Label]
pinotNoir = "Pinot Noir" :: Grape

