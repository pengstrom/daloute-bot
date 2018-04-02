module Dice
  ( Dice
  , Roll
  , diceType
  , diceNum
  , modValue
  , rollDices
  , rollLabel
  , mkDice
  , mkmDice
  , mkMod
  , mkRoll
  , mklRoll
  ) where

-- | A dice is either a number of rolls of a n-sided die or a constant modifier.
data Dice
  = Dice { diceType :: Int -- ^ The number of sides of the dice.
         , diceNum  :: Maybe Int -- ^ Number of times the dice is rolled.
          }
  | Mod { modValue :: Int -- ^ Modifier value.
         }
  deriving (Eq, Show)

-- | A roll consists of one or more dice and an optional label.
data Roll = Roll
  { rollDices :: [Dice] -- ^ The dices of the roll.
  , rollLabel :: Maybe String -- ^ The roll label.
  } deriving (Eq, Show)

mkDice :: Int -> Dice
mkDice n = Dice n Nothing

mkmDice :: Int -> Int -> Dice
mkmDice n m = Dice n (Just m)

mkMod :: Int -> Dice
mkMod = Mod

mkRoll :: [Dice] -> Roll
mkRoll dices
  | null dices = error "Requires at least one dice"
  | otherwise = Roll dices Nothing

mklRoll :: [Dice] -> String -> Roll
mklRoll dices label
  | null dices = error "Requires at least one dice"
  | otherwise = Roll dices (Just label)
