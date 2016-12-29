module Lib where

import Data.Maybe
import Control.Monad
import Text.Regex
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

getCardTags :: FilePath -> IO (Set.Set String)
getCardTags p = Set.fromList . lines <$> readFile p

playerKRegex :: Regex
playerKRegex = mkRegexWithOpts "^([^,]*),(.*)$" False True

parsePositiveInteger :: String -> Maybe Integer
parsePositiveInteger s = listToMaybe $ fst <$> (reads s :: [(Integer, String)])

type SNum = (String, Integer)

parsedToEntry :: [String] -> Maybe SNum
parsedToEntry ss = curry id name <$> parsePositiveInteger k_s
  where (name:k_s:_) = ss

type SNumP = Either String SNum

parseLine :: Integer -> String -> SNumP
parseLine num line = case joined of
  Nothing -> Left $ "line " ++ (show num) ++ ": '" ++ line ++ "' is invalid"
  Just x -> Right x
  where joined = join $ parsedToEntry <$> matchRegex playerKRegex line

type KMap = Map.Map String Integer
type KMapP = Either String KMap

validatePlayer :: KMap -> SNum -> KMapP
validatePlayer mp (name, k) =
  if k <= 0 then Left $ "player '" ++ name ++ "' has invalid k=" ++ show k
  else maybe (Right $ Map.insert name k mp) nexists (Map.lookup name mp)
  where nexists x = Left (
          "player '" ++ name ++ "' already exists with k=" ++ show x)

type NumberAndLine = (Integer, String)

collectPlayerK :: KMapP -> NumberAndLine -> KMapP
collectPlayerK mp (num, line) = join $ return validatePlayer `ap` mp `ap` parsed
  where parsed = parseLine num line

parsePlayers :: [String] -> KMapP
parsePlayers ss = foldl collectPlayerK (Right Map.empty) $ zip [1..] ss

getPlayerK :: FilePath -> IO KMapP
getPlayerK p = parsePlayers . lines <$> readFile p

someFunc :: IO ()
someFunc = putStrLn "someFunc"
