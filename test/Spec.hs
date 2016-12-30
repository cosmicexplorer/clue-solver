import Test.HUnit

import Control.Monad

import ConfigGame

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

test1 = TestCase (
  assertEqual "reads a cards file" mock =<< getCardTags "test/test-input")
  where mock = Set.fromList . map (Card . show) $ [1,2,4,6,9]

transformElNaive :: (String, Integer) -> (PlayerName, PlayerK)
transformElNaive (s, k) = ((PlayerName s), (PlayerK k))

kmapFromList :: [(String, Integer)] -> KMap
kmapFromList = KMap . Map.fromList . map transformElNaive

test2 = TestCase (
  assertEqual "reads a players file" mock =<< getPlayerK "test/test-player-k")
  where mock = Right $ kmapFromList [("s",1),("m",3),("p",4)]

test3 = TestCase (do
    p1 <- getPlayerK "test/test-k-fail-no-parse"
    assertEqual "errors on parse failure" mock1 p1
    p2 <- getPlayerK "test/test-k-fail-not-integer"
    assertEqual "errors on integer parsing" mock2 p2
    p3 <- getPlayerK "test/test-k-fail-same-name"
    assertEqual "errors on same name" mock3 p3)
  where mock1 = Left "line 3: 'as' is invalid"
        mock2 = Left "line 3: 'c,12.3' is invalid"
        mock3 = Left "player 's' already exists with k=1"

tests = TestList [TestLabel "cards" test1,
                  TestLabel "player-k" test2,
                  TestLabel "player-k-fail" test3]

main :: IO ()
main = void $ runTestTT tests
