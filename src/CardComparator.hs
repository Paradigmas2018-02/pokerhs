module CardComparator(
    compareCard
)where

import Deck (Card(..), Rank(..), Suit(..))

-- Compare two cards and return the strongest
compareCard:: Card -> Card -> Card 
compareCard card1 card2
        | rvalue (rank card1) > rvalue (rank card2) = card1
        | rvalue (rank card1) < rvalue (rank card2) = card2
        | rvalue (rank card1) == rvalue(rank card2) = compareSuit card1 card2

-- Compare two cards and return the strongest suit
compareSuit:: Card -> Card -> Card
compareSuit card1 card2
    | svalue (suit card1) > svalue (suit card2) = card1
    | svalue (suit card1) < svalue (suit card2) = card2 
    | otherwise = error "Cards have the same suit"
