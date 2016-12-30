module GameOps where

import ConfigGame

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type CardMap = Map.Map Card Bool
type KnownCards = Set.Set Card
-- TODO: make this a set of sets instead of array?
type PossibleSets = [KnownCards]

data PlayerState = Intermediate CardMap PlayerK PossibleSets
                 | Complete KnownCards

parseCommand :: String -> (PlayerState -> PlayerState)
parseCommand s = id
