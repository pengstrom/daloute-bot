module Dice.ParserSpec
  ( main
  , spec
  ) where

import           Test.Hspec
import           Test.QuickCheck

import           Dice.Internal
import           Dice.Parser

import           Data.Maybe

import           Control.Monad

tests :: [(String, Roll)]
tests =
  [ ("5 Ongoing", Roll [Mod 5] (Just "Ongoing"))
  , ("d20", Roll [Dice 20 Nothing] Nothing)
  , ("2d6+3d12", Roll [Dice 6 (Just 2), Dice 12 (Just 3)] Nothing)
  , ("d20 + 15", Roll [Dice 20 Nothing, Mod 15] Nothing)
  , ( " 15 + d20 + 2d16  Dawizard  :) "
    , Roll [Mod 15, Dice 20 Nothing, Dice 16 (Just 2)] (Just "Dawizard :)"))
  ]

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Parser" $ do
    it "Rejects empty string" $ isNothing $ parseRoll ""
    it "Rejects only label" $ isNothing $ parseRoll " a label "
    it "Parses integers" $
      property $ \(NonNegative x) ->
        parseRoll (show x) == Just (Roll [Mod x] Nothing)
    forM_ tests (\(s, r) -> it ("Parses '" ++ s ++ "'") $ parseRoll s == Just r)
