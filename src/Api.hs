{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Poker(flop)
import Deck(deck, remainingDeck)

main = do
    f <- flop deck
    scotty 3000 $ do
        get "/deck" $ 
            json deck

        get "/flop" $
             json f
