{-# LANGUAGE DeriveGeneric #-}

module Deck (
    Card(..),
    Rank(..),
    Suit(..),
    deck,
    pick,
    remainingDeck,
    remainingCards,
) where

import System.Random
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

-- A rank of a card has a value and a name
data Rank = Rank { rvalue :: Int, rname :: String} deriving (Generic, Show, Eq, Ord)

-- A card suit has a value and a name
data Suit = Suit { svalue :: Int, sname::String} deriving (Generic, Show, Eq, Ord)

-- A card that have a rank and a suit e.g. Card (Rank {1, "Ace"}, "Spades") --
data Card = Card { rank :: Rank , suit :: Suit } deriving (Generic, Show, Eq, Ord)

instance ToJSON Rank
instance FromJSON Rank

instance ToJSON Suit
instance FromJSON Suit

instance ToJSON Card
instance FromJSON Card

-- Generate all card ranks for a card e.g. Rank {value=1, name="ace"}
ranks =
    [ Rank{rvalue = fst x, rname = snd x} | x <- tuples ]
    where 
        tuples = zip [2..14] ["Two", "Three", "Four", "Five", "Six", "Seven", "Eight", 
                              "Nine", "Ten", "Jack", "Queen", "King", "Ace"]

        
-- Generate all card suits for a card e.g. Suit {value=1, name="Clubs"}
suits =
    [ Suit{svalue = fst x, sname = snd x} | x <- tuples ]
    where 
        tuples = zip [1..4] [ "Clubs", "Hearts", "Diamonds", "Spades"]

-- Generates a full deck of cards for a texas holden poker game --
deck :: [Card]
deck = [Card {rank=x, suit=y} |
                x <- ranks,
                y <- suits
        ]


-- Pick a random card from a card deck --
--      >> xs - a list of cards
pick :: [Card] -> IO Card
pick xs = do
    i <- randomRIO(0, length xs-1)
    return (xs !! i)



          

-- Return a deck with all remaining cards on a deck --
--      >> xs - a list with already used cards
remainingDeck :: IO [Card] -> IO [Card]
remainingDeck xs = do
    ys <- xs 
    return [y | y <- deck, y `notElem` ys]


-- Return a list of cards that have not been picked yet  --
--      >> xs - a list of cards
--      >> ys - a list with already picked cards
remainingCards xs ys = [x | x <- xs, x `notElem` ys]

