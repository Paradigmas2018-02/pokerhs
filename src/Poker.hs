{-# LANGUAGE DeriveGeneric #-}

module Poker (
    flop,
    -- newRoles,
    giveCards,
    Hand(..),
    Table(..),
) where

import GHC.Generics
import Data.Aeson (ToJSON)
import Deck (Card,Rank, pick, remainingCards)
import Utils(development, createTuples)
import System.Process
import System.IO
import Data.Char
import Control.Monad

-- A hand in a texas holden poker --
data Hand = Hand { hvalue :: Rank, hcards :: [Card]} deriving (Generic, Show, Eq, Ord)
instance ToJSON Hand

data Table = Table { tcards :: [Card], pot :: Int, thand :: Hand } deriving (Generic, Show)
instance ToJSON Table


flop :: [Card] -> IO [Card]
flop xs =
    core xs []
    where core xs c_acc = do
            c <- pick xs
            if length c_acc == 3
                then return c_acc
            else
                core (remainingCards xs (c:c_acc)) (c:c_acc)

giveCards :: [Card] -> Int -> IO [(Card, Card)]
giveCards xs n =
    core xs (n*2) []
    where core xs n c_acc = do
            c <- pick xs
            if n == 0
                then return (createTuples c_acc)
            else
                core (remaining xs c_acc c) (n-1) (c:c_acc)

          remaining xs ys e = [x | x <- xs, x /= e, e `notElem` ys]