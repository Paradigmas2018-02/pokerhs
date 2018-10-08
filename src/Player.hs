{-# LANGUAGE DeriveGeneric #-}

module Player (
    Player(..),
    bet,
    -- charge,
) where

import GHC.Generics
import Data.Aeson (ToJSON)
import Deck (Card)
import Poker(Hand)

data Player = Player { name :: String, tokens :: Int, cards :: (Card, Card), phand :: Hand, payment :: Int } deriving (Generic, Show, Eq)
instance ToJSON Player

-- Make a bet to the game --
--      >> pl - player that made the bet
--      >> b - value of the bet
--      >> p - actual pot of tokens
bet :: Player -> Int -> Int -> (Player, Int)
bet pl b p = (charge pl b, p + b)


-- -- Charge a bet value from the player --
-- --      >> p - player that will be charged
-- --      >> b - the value of the charge
charge :: Player -> Int -> Player
charge p b
    | tokens p == 0 = error "This player can't bet because he has 0 tokens"
    | tokens p < b = Player {name = name p, tokens = 0, cards = cards p, phand = phand p, payment = payment p + b}
    | otherwise = Player {name = name p, tokens = tokens p - b, cards = cards p, phand = phand p, payment = payment p + b}
