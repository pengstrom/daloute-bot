module Dice.RollerSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Dice.Internal
import           Dice.Roller

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Roller" $ do
    it "Is constant for Mod" $
      property $ \(NonNegative x) -> rollDice (mkMod x) `shouldReturn` x
    it "Is bounded" $
      property $ \(Positive x) ->
        monadicIO $ do
          r <- run (rollDice (mkDice x))
          assert $ r <= x && r >= 1
    it "Is bounded for many" $
      property $ \(Positive x) (Positive y) ->
        monadicIO $ do
          r <- run (rollDice (mkmDice x y))
          assert $ r <= x * y && r >= 1 * y
