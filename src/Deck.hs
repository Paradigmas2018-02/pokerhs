module Deck (
    Card(..),
    Hand(..),
    Rank(..),
    deck,
    pick,
    hand,
    remainingDeck,
    remainingCards,
    splitHands,
    splitHand,
    createHand,
    canMakeHand,
    ranks,
) where

import System.Random

-- A rank of a card has a value and a name
data Rank = Rank { value :: Int, name :: String} deriving (Show, Eq)

-- A card that have a rank and a suit e.g. Card (Rank {1, "Ace"}, "Spades") --
data Card = Card { rank :: Rank , suit :: String } deriving (Show, Eq)

-- A hand in a texas holden poker --
data Hand = Hand Card Card deriving (Show, Eq)


-- Generate all Card Ranks for a Card e.g. Rank {value=1, name="ace"}
ranks =
    [ Rank{value = fst x, name = snd x} | x <- tuples ]
    where 
        tuples = zip [1..13] ["Ace", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King"]

        
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


hand :: [Card] -> Int -> IO [Hand]
hand xs n = 
    core xs n [] []
    where core xs n c_acc h_acc = do
            c <- pick xs
            if n == 0
                then return h_acc
            else if canMakeHand c_acc
                then core (remaining xs c_acc c) (n-1) [] (createHand c_acc:h_acc)
            else
                core (remaining xs c_acc c) n (c:c_acc) h_acc

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


-- Convert a list of hands of cards to a list of cards --
--      >> xs - a list of hands
splitHands :: IO [Hand] -> IO [Card]
splitHands xs = do
    ys <- xs
    return $ concat [splitHand x | x <- ys]


-- Convert one unique hand into a list of cards --
splitHand :: Hand -> [Card]
splitHand (Hand c1 c2) = [c1, c2]


-- Create a hand with the two first elements on a list --
createHand :: [Card] -> Hand
createHand [] = error "You must provide at least two cards for making a hand"
createHand [n] = error "You must provide at least two cards for making a hand"
createHand x = Hand (head x) (head (tail x))


-- Verify if a list of cards can generate a hand --
canMakeHand :: [Card] -> Bool
canMakeHand xs
    | null xs = False
    | otherwise = length xs >= 2