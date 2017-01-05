module GameOps where

import ConfigGame

import Data.List
import Control.Monad
import Text.Printf
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Text.Parsec.Prim as Prim
import Text.ParserCombinators.Parsec
import Text.EditDistance

type KnownCards = Set.Set Card
type PossibleSets = Set.Set KnownCards
type AllPlayerStates = Map.Map PlayerName PlayerState

data PlayerState = Intermediate KnownCards PlayerK PossibleSets
                 | Complete KnownCards
newtype GameState = GameState AllPlayerStates

data CommandResult = CommandResult GameState String

line :: ValidateCardOpts -> ValidatePlayerOpts -> GenParser Char st String
line copts popts = (
  Prim.try (query copts) <|>
  Prim.try (failure popts) <|>
  Prim.try (success popts) <|>
  Prim.try (cardKnown copts popts) <|>
  Prim.try display
  ) <* eof

eol :: GenParser Char st Char
eol = char '\n'

parseRepl :: ValidateCardOpts -> ValidatePlayerOpts -> String -> Either ParseError String
parseRepl copts popts = parse (line copts popts) "(lol)"

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

validateCard :: (CardAndAlternatives -> t) -> ValidateCardOpts -> Card -> Either t Card
validateCard f opts card
  | Set.notMember card allCards =
    Left . f $ CardAndAlternatives card $ findLevenAlternatives opts card
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

makeSimilarCardsMsg :: Set.Set Card -> Card -> String
makeSimilarCardsMsg set (Card c) =
  printf "card '%s' was not found; did you mean: [%s]?" c setstr
  where setstr = intercalate "," $ map uncard . Set.toList $ set
        uncard (Card d) = d

makeAllSimilarCardsMsg :: ([CardAndAlternatives], [Card]) -> Either String (Set.Set Card)
makeAllSimilarCardsMsg (cals, cs) = case cals of
  [] -> Right $ Set.fromList cs
  x -> Left $ intercalate "\n" $ map makeMsg cals
  where makeMsg (CardAndAlternatives c alts) = makeSimilarCardsMsg alts c

query :: ValidateCardOpts -> GenParser Char st String
query opts = result >>= either unexpected (parserReturn . format)
  where cards = string "q:" *> sepBy1 (many $ noneOf ",\n") (char ',')
        unique slst = case uniqueList $ map Card slst of
          Left s -> Left $ makeDupCardsMsg s
          Right x -> Right x
        parted = makeAllSimilarCardsMsg . splitMap (validateCard id opts)
        result = (=<<) parted . unique <$> cards
        format = printf "query: %s" .
          intercalate "," .
          map (printf "<%s>" . uncard) .
          Set.toList

data ValidatePlayerOpts = ValidatePlayerOpts (Set.Set PlayerName)

validPlayer :: ValidatePlayerOpts -> PlayerName -> Either String PlayerName
validPlayer (ValidatePlayerOpts set) name
  | Set.member name set = Right name
  | otherwise = Left $ printf "player %s' not found"

unplayer :: PlayerName -> String
unplayer (PlayerName s) = s

failure :: ValidatePlayerOpts -> GenParser Char st String
failure opts = valid >>= either unexpected (parserReturn . failFormat)
  where p = string "f:" *> many (noneOf "\n")
        valid = validPlayer opts . PlayerName <$> p
        failFormat = printf "fail:<%s>" . unplayer

success :: ValidatePlayerOpts -> GenParser Char st String
success opts = valid >>= either unexpected (parserReturn . successFormat)
  where p = string "s:" *> many (noneOf "\n")
        valid = validPlayer opts . PlayerName <$> p
        successFormat = printf "success:<%s>" . unplayer

cardKnown :: ValidateCardOpts -> ValidatePlayerOpts -> GenParser Char st String
cardKnown copts popts = p >>= either unexpected (parserReturn . cardKnownFmt)
  where validate c n = do
          card <- validateCard makeSimilarMsg copts (Card c)
          name <- validPlayer popts . PlayerName $ n
          return (card, name)
        makeSimilarMsg (CardAndAlternatives c cs) = makeSimilarCardsMsg cs c
        (ValidateCardOpts (AllCardSet cset) _) = copts
        p = do
          string "c:"
          card <- many $ noneOf ",\n"
          char ','
          name <- many $ noneOf ",\n"
          return $ validate card name
        cardKnownFmt (Card c, PlayerName n) =
          printf "known: card=%s,name=%s" c n

display :: GenParser Char st String
display = string "d" *> parserReturn "display"
