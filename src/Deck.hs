module Deck (
    Card(..),
    Hand(..),
    Rank(..),
    deck,
    pick,
    giveCards,
    remainingDeck,
    remainingCards,
    splitCardTuples,
) where

import System.Random
import Utils (createTuples, destroyTuples)

-- A rank of a card has a value and a name
data Rank = Rank { value :: Int, name :: String} deriving (Show, Eq)

-- A card that have a rank and a suit e.g. Card (Rank {1, "Ace"}, "Spades") --
data Card = Card { rank :: Rank , suit :: String } deriving (Show, Eq)

-- A hand in a texas holden poker --
data Hand = Hand Card Card deriving (Show, Eq)


-- Generate all card ranks for a card e.g. Rank {value=1, name="ace"}
ranks =
    [ Rank{value = fst x, name = snd x} | x <- tuples ]
    where 
        tuples = zip [1..13] ["Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", 
                              "Nine", "Ten", "Jack", "Queen", "King"]

        
-- Generates a full deck of cards for a texas holden poker game --
deck :: [Card]
deck = [Card {rank=x, suit=y} |
                x <- ranks,
                y <- ["Clubs", "Diamonds", "Hearts", "Spades"]
        ]


-- Pick a random card from a card deck --
--      >> xs - a list of cards
pick :: [Card] -> IO Card
pick xs = do
    i <- randomRIO(0, length xs-1)
    return (xs !! i)


-- Give cards for n players --
--      >> xs - a list of available cards
--      >> n - the number of players
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


-- Convert a list of card tuples to a list of cards --
--      >> xs - a list of card tuples
splitCardTuples :: IO [(Card, Card)] -> IO [Card]
splitCardTuples xs = destroyTuples <$> xs