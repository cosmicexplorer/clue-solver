import Test.HUnit

import Control.Monad

import Lib

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

test1 = TestCase (
  assertEqual "reads a cards file" mock =<< getCardTags "test/test-input")
  where mock = (Set.fromList . map show $ [1,2,4,6,9])


test2 = TestCase (
  assertEqual "reads a players file" mock =<< getPlayerK "test/test-player-k")
  where mock = Right $ Map.fromList [("s",1),("m",3),("p",4)]

tests = TestList [TestLabel "cards" test1, TestLabel "player-k" test2]

main :: IO ()
main = void $ runTestTT tests
