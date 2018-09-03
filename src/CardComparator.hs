module CardComparator(
    compareCard
)where

import Deck (Card(..), Rank(..))

-- Compare two cards and return the strongest
compareCard:: Card -> Card -> Card 
compareCard card1 card2
        | value (rank card1) > value (rank card2) = card1
        | value (rank card1) < value (rank card2) = card2
        | otherwise = card1