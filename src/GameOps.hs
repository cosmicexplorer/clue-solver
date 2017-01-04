module GameOps where

import ConfigGame

import Data.List
import Control.Monad
import Text.Printf
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Text.ParserCombinators.Parsec
import Text.EditDistance

type KnownCards = Set.Set Card
type PossibleSets = Set.Set KnownCards
type AllPlayerStates = Map.Map PlayerName PlayerState

data PlayerState = Intermediate KnownCards PlayerK PossibleSets
                 | Complete KnownCards
newtype GameState = GameState AllPlayerStates

data CommandResult = CommandResult GameState String

repl :: ValidateCardOpts -> GenParser Char st [String]
repl opts = many (line opts) <* eof

line :: ValidateCardOpts -> GenParser Char st String
line opts = query opts <|> many (noneOf "\n") <* eol

newtype LevenDist = LevenDist Int
  deriving (Eq, Show, Ord)

data ValidateCardOpts = ValidateCardOpts {
  set :: AllCardSet,
  dist :: LevenDist
} deriving (Eq, Show)

getLevenDist :: LevenDist -> Int
getLevenDist (LevenDist dist)
  | dist < 0 = 0
  | otherwise = dist

data CardAndAlternatives = CardAndAlternatives Card (Set.Set Card)
  deriving Show

findLevenAlternatives :: ValidateCardOpts -> Card -> Set.Set Card
findLevenAlternatives (ValidateCardOpts set dist) (Card c) =
  Set.filter similar cards
  where (AllCardSet cards) = set
        fixedDist = getLevenDist dist
        similar (Card d) = fixedDist >= levenshteinDistance defaultEditCosts d c

validateCard :: ValidateCardOpts -> Card -> Either CardAndAlternatives Card
validateCard opts card
  | Set.notMember card allCards =
    Left $ CardAndAlternatives card $ findLevenAlternatives opts card
  | otherwise = Right card
  where AllCardSet allCards = set opts

-- TODO: use hash so we don't need Ord?
uniqueList :: Ord a => [a] -> Either (Set.Set a) [a]
uniqueList lst
  | null dup = Right lst
  | otherwise = Left all
  where exists (dup, all) el
          | Set.member el all = (Set.insert el dup, all)
          | otherwise = (dup, Set.insert el all)
        (dup, all) = foldl exists (Set.empty, Set.empty) lst

type EitherPartition t a b = t -> Either a b

splitMap :: EitherPartition t a b -> [t] -> ([a], [b])
splitMap f lst = res
  where mapped = map f lst
        res = foldl part ([], []) mapped
        part (l, r) e = case e of
          Left x -> (x:l, r)
          Right y -> (l, y:r)

uncard :: Card -> String
uncard (Card c) = c

makeDupCardsMsg :: Set.Set Card -> String
makeDupCardsMsg set = printf "The following cards were duplicated: [%s]" setstr
  where setstr = intercalate "," $ map uncard . Set.toList $ set

makeSimilarCardsMsg :: Card -> Set.Set Card -> String
makeSimilarCardsMsg (Card c) set =
  printf "card '%s' was not found; did you mean: [%s]?" c setstr
  where setstr = intercalate "," $ map uncard . Set.toList $ set
        uncard (Card d) = d

makeAllSimilarCardsMsg :: ([CardAndAlternatives], [Card]) -> Either String (Set.Set Card)
makeAllSimilarCardsMsg (cals, cs) = case cals of
  [] -> Right $ Set.fromList cs
  x -> Left $ intercalate "\n" $ map makeMsg cals
  where makeMsg (CardAndAlternatives c alts) = makeSimilarCardsMsg c alts

query :: ValidateCardOpts -> GenParser Char st String
query opts = (++) "query:" . intercalate "," . map (\s -> "<" ++ s ++ ">") <$> cards
  where cards = string "q:" *> sepBy1 (many $ noneOf ",\n") (char ',') <* eol
        unique slst = case uniqueList $ map Card slst of
          Left s -> Left $ makeDupCardsMsg s
          Right x -> Right x
        parted = makeAllSimilarCardsMsg . splitMap (validateCard opts)
        result = (=<<) parted . unique <$> cards

eol :: GenParser Char st Char
eol = char '\n'

parseRepl :: ValidateCardOpts -> String -> Either ParseError [String]
parseRepl opts = parse (repl opts) "(lol)"

-- parseCommand :: String -> GameState -> CommandResult
