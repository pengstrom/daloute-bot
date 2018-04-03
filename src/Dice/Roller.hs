module Dice.Roller
  ( roll
  , rollDice
  ) where

import           Dice.Internal
import           System.Random

roll :: Roll -> IO Integer
roll (Roll ds _) = sum <$> mapM rollDice ds

rollDice :: Dice -> IO Integer
rollDice (Mod x) = return x
rollDice (Dice n mm) = do
  x <- randomRIO (1, n)
  return $ maybe x (* x) mm
