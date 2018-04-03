module Dice.Roller
  ( present
  , roll
  , rollDice
  ) where

import           Dice.Internal
import           System.Random

import           Dice.Parser

import           Control.Monad
import           Data.Maybe

present :: String -> IO String
present spec =
  case parseRoll spec of
    Nothing -> return "Invalid format"
    Just r -> do
      x <- roll r
      let s = "You rolled " ++ show x
      return $ maybe s (\l -> s ++ " for " ++ l) $ rollLabel r

roll :: Roll -> IO Integer
roll (Roll ds _) = sum <$> mapM rollDice ds

rollDice :: Dice -> IO Integer
rollDice (Mod x) = return x
rollDice (Dice n mm) = do
  let m = fromInteger $ fromMaybe 1 mm
  sum <$> replicateM m (randomRIO (1, n))
