module Dice.Parser.Internal where

import           Dice

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec () String

parseRoll :: String -> Maybe Roll
parseRoll = parseMaybe rollParser

rollParser :: Parser Roll
rollParser = do
  space
  ds <- diceParser' `sepBy1` try (space >> char '+' >> space)
  space
  ms <- optional labelParser
  eof
  return $
    case ms of
      Just s  -> mklRoll ds s
      Nothing -> mkRoll ds

labelParser :: Parser String
labelParser = do
  l <- some anyChar
  return $ (unwords . words) l

diceParser' :: Parser Dice
diceParser' = try diceParser <|> modParser

diceParser :: Parser Dice
diceParser = do
  mm <- optional decimal
  char 'd'
  n <- decimal
  return $
    case mm of
      Just m  -> mkmDice n m
      Nothing -> mkDice n

modParser :: Parser Dice
modParser = do
  n <- decimal
  return $ mkMod n
