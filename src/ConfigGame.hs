module ConfigGame where

import Data.Maybe
import Control.Monad
import Text.Regex
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

newtype Card = Card String
  deriving (Ord, Eq, Show)

getCardTags :: FilePath -> IO (Set.Set Card)
getCardTags p = Set.fromList . map Card . lines <$> readFile p

playerKRegex :: Regex
playerKRegex = mkRegexWithOpts "^([^,]*),(.*)$" False True

parseInteger :: String -> Maybe Integer
parseInteger s = listToMaybe $ fst <$> (reads s :: [(Integer, String)])

type SNum = (String, Integer)

parsedToEntry :: [String] -> Maybe SNum
parsedToEntry ss = curry id name <$> parseInteger k_s
  where (name:k_s:_) = ss

type SNumP = Either String SNum

parseLine :: Integer -> String -> SNumP
parseLine num line = case joined of
  Nothing -> Left $ "line " ++ show num ++ ": '" ++ line ++ "' is invalid"
  Just x -> Right x
  where joined = join $ parsedToEntry <$> matchRegex playerKRegex line

newtype PlayerName = PlayerName String
  deriving (Eq, Ord, Show)
newtype PlayerK = PlayerK Integer
  deriving (Eq, Ord, Show)

newtype KMap = KMap (Map.Map PlayerName PlayerK)
  deriving (Eq, Ord, Show)
type KMapP = Either String KMap

validatePlayer :: KMap -> SNum -> KMapP
validatePlayer m (name, k)
  | k <= 0 = Left $ "player '" ++ name ++ "' has invalid k=" ++ show k
  | otherwise = case Map.lookup pname mp of
      Nothing -> Right $ KMap $ Map.insert pname (PlayerK k) mp
      Just x -> Left (
        "player '" ++ name ++ "' already exists with k=" ++ show x)
  where KMap mp = m
        pname = PlayerName name

type NumberAndLine = (Integer, String)

collectPlayerK :: KMapP -> NumberAndLine -> KMapP
collectPlayerK mp (num, line) = join $ return validatePlayer `ap` mp `ap` parsed
  where parsed = parseLine num line

parsePlayers :: [String] -> KMapP
parsePlayers ss = foldl collectPlayerK (Right $ KMap Map.empty) $ zip [1..] ss

getPlayerK :: FilePath -> IO KMapP
getPlayerK p = parsePlayers . lines <$> readFile p
