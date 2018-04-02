module DiceSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Dice

import           Data.Maybe

import           Control.Exception

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Dice" $ do
    it "Constructs Dice type" $ property $ \x -> diceType (mkDice x) == x
    it "Constructs Dice number" $
      property $ \x -> isNothing (diceNum (mkDice x))
    it "Constructs many Dice type" $
      property $ \x y -> diceType (mkmDice x y) == x
    it "Constructs many Dice number" $
      property $ \x y -> diceNum (mkmDice x y) == Just y
  describe "Mod" $
    it "Constructs Mod" $ property $ \x -> modValue (mkMod x) == x
  describe "Roll" $ do
    it "Fails empty" $
      evaluate (mkRoll []) `shouldThrow` errorCall "Requires at least one dice"
    it "Constructs Roll" $
      property $ \x ->
        let ds = [mkDice x]
        in rollDices (mkRoll ds) == ds
    it "Constructs Roll label" $
      property $ \x ->
        let ds = [mkDice x]
        in isNothing (rollLabel (mkRoll ds))
    it "Constructs labeled Roll label" $
      property $ \x s ->
        let ds = [mkDice x]
        in rollLabel (mklRoll ds s) == Just s
