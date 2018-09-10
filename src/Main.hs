

import Player
import Deck
import Poker
import Utils
import Find

initialTokens = 200
playerCards = giveCards deck 6

players = do
    xs <- playerCards
    return [
        Player { name="Daniel Negreanu", tokens=initialTokens, cards=head xs},
        Player { name="Erik Seidel", tokens=initialTokens, cards=xs !! 1},
        Player { name="Fedor Holz", tokens=initialTokens, cards=xs !! 2},
        Player { name="Phil Ivey", tokens=initialTokens, cards=xs !! 3},
        Player { name="John Juanda", tokens=initialTokens, cards=xs !! 4},
        Player { name="You", tokens=initialTokens, cards=xs !! 5}
        ]
