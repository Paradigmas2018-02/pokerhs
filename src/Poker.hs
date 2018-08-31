module Poker (
    flop,
) where

import Deck (Card, pick, remainingCards)

flop :: IO [Card] -> IO [Card]
flop xs = do
    ys <- xs
    core ys []
    where core xs c_acc = do
            c <- pick xs
            if length c_acc == 3
                then return c_acc
            else
                core (remainingCards xs (c:c_acc)) (c:c_acc)
