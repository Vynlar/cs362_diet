module Food
    ( calculateFood
    , foods
    , Food(cost)
    , nutrientBounds
    ) where

import Numeric.LinearProgramming
import Data.List
import Text.PrettyPrint.Boxes
import Text.Printf

data Food = Food { name :: String
                 , cost :: Double -- dollars
                 , mass :: Double -- grams
                 , calories :: Double
                 , satFat :: Double -- grams
                 , sodium :: Double -- milligrams
                 , vitC :: Double -- milligrams
                 , vitA :: Double -- micrograms
                 , protein :: Double -- grams
                 } deriving (Show)

foods :: [Food]
foods =
  [ Food { name="Avocado"
         , cost=1.58
         , mass=214
         , calories=318
         , satFat=4
         , sodium=12
         , vitC=26
         , vitA=15
         , protein=4.4
         }
  , Food { name="Steak"
         , cost=4.12
         , mass=182
         , calories=379
         , satFat=8
         , sodium=131
         , vitC=0
         , vitA=0
         , protein=46
         }
  , Food { name="Ice Cream"
         , cost=0.97
         , mass=107
         , calories=194
         , satFat=5
         , sodium=80
         , vitC=1.1
         , vitA=113
         , protein=4.3
         }
  , Food { name="Kool-Aid sugar-free grape flavored drink mix"
         , cost=1.67
         , mass=0.827
         , calories=5
         , satFat=0
         , sodium=10
         , vitC=6
         , vitA=0
         , protein=0
         }
  , Food { name="Burger King cini-mini roll"
         , cost=1.99
         , mass=30
         , calories=111
         , satFat=1
         , sodium=146
         , vitC=0
         , vitA=0
         , protein=1.8
         }
  , Food { name="Broccoli"
         , cost=0.56
         , mass=85
         , calories=26
         , satFat=42
         , sodium=25
         , vitC=78
         , vitA=57
         , protein=2.5
         }
  , Food { name="Egg"
         , cost=1.66/12 -- 1.66 per dozen eggs
         , mass=35
         , calories=55
         , satFat=1
         , sodium=65
         , vitC=0
         , vitA=61
         , protein=4.1
         }
  ]

opt :: Optimization
opt = Minimize $ map cost foods 

constraints :: Constraints
constraints = Sparse nutrientBounds

nutrientBounds :: [Bound [(Double, Int)]]
nutrientBounds =
  [ ((map (\(food, i) -> calories food # i)) $ zip foods [1..]) :==: 2000
  , ((map (\(food, i) -> satFat food # i)) $ zip foods [1..]) :<=: 20
  , ((map (\(food, i) -> sodium food # i)) $ zip foods [1..]) :<=: 2400
  , ((map (\(food, i) -> vitC food # i)) $ zip foods [1..]) :>=: 90
  , ((map (\(food, i) -> vitA food # i)) $ zip foods [1..]) :>=: 700
  , ((map (\(food, i) -> protein food # i)) $ zip foods [1..]) :>=: 56
  ]

diet :: Solution
diet = simplex opt constraints []

nutrients = [ (cost, "Cost") -- not a nutrient per say, but still part of our report
            , (calories, "Calories")
            , (satFat, "Saturated Fat")
            , (sodium, "Sodium")
            , (vitC, "Vitamin C")
            , (vitA, "Vitamin A")
            , (protein, "Protein")
            ]

summary :: Solution -> [[String]]
summary (Optimal (_, amounts)) =
  [ "Food" : "Grams" : map snd nutrients ]
  ++ (map makeRow $ zip amounts foods)
  ++ blankLine
  ++ [totals]
  where
    makeRow :: (Double, Food) -> [String]
    makeRow (amount, food) =
      (name food) : (printFloat (amount * mass food)) :
        (( map printFloat
        . map (\(getNutrient, _) -> amount * getNutrient food)
        ) nutrients)

    totals :: [String]
    totals =
      [ "Total"
      , (printFloat . sum . map (\(amount, food) -> amount * mass food)) $ zip amounts foods
      ]
      ++ (map makeTotal nutrients)

    makeTotal :: ((Food -> Double), String) -> String
    makeTotal (getNutrient, _) =
      (printFloat . sum . map (\(food, amount) -> amount * getNutrient food)) $ zip foods amounts

    printFloat = printf "%.2f"

    blankLine =
      [replicate (2 + length nutrients) "-----"]
summary _ = error "No feasible diet exists."

calculateFood :: IO ()
calculateFood = printTable $ summary diet

printTable :: [[String]] -> IO ()
printTable rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

main :: IO ()
main = calculateFood
