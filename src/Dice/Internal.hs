module Dice.Internal where

import           Data.List

-- | A dice is either a number of rolls of a n-sided die or a constant modifier.
data Dice
  = Dice { diceType :: Integer -- ^ The number of sides of the dice.
         , diceNum  :: Maybe Integer -- ^ Number of times the dice is rolled.
          }
  | Mod { modValue :: Integer -- ^ Modifier value.
         }
  deriving (Eq)

-- | A roll consists of one or more dice and an optional label.
data Roll = Roll
  { rollDices :: [Dice] -- ^ The dices of the roll.
  , rollLabel :: Maybe String -- ^ The roll label.
  } deriving (Eq)

instance Show Dice where
  show (Mod x) = show x
  show (Dice n mm) =
    let s = "d" ++ show n
    in case mm of
         Just m  -> show m ++ s
         Nothing -> s

instance Show Roll where
  show (Roll ds ml) =
    let s = intercalate " + " (map show ds)
    in case ml of
         Just l  -> s ++ " " ++ l
         Nothing -> s

mkDice :: Integer -> Dice
mkDice n = Dice n Nothing

mkmDice :: Integer -> Integer -> Dice
mkmDice n m = Dice n (Just m)

mkMod :: Integer -> Dice
mkMod = Mod

mkRoll :: [Dice] -> Roll
mkRoll dices
  | null dices = error "Requires at least one dice"
  | otherwise = Roll dices Nothing

mklRoll :: [Dice] -> String -> Roll
mklRoll dices label
  | null dices = error "Requires at least one dice"
  | otherwise = Roll dices (Just label)
