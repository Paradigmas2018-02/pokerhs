import System.Random

-- A card that have a rank and a suit e.g. ("Ace", "Spades") --
data Card = Card { rank :: String, suit :: String} deriving (Show, Eq)

-- A hand in a texas holden poker --
data Hand = Hand Card Card Card deriving (Show, Eq)

-- Generates a full deck of cards for a texas holden poker game --
deck :: [Card]
deck = [Card {rank=x, suit=y} |
            x <- ["Ace", "Two", "Three", "Four", "Five", "Six", "Seven",
                    "Eight", "Nine", "Ten", "Jack", "Queen", "King"],
            y <- ["Clubs", "Diamonds", "Hearts", "Spades"]
        ]

-- Pick a random card from a card deck --
pick :: [Card] -> IO Card
pick xs = do
    i <- randomRIO(0, (length xs)-1)
    return (xs !! i)

-- Return n texas holden poker hands of cards  --
--      >> xs - a deck with all cards of texas holden poker
--      >> n - how many hands to generate
hand :: [Card] -> Int -> IO [Hand]
hand xs n = do
    hand' xs n [] [] []

-- Auxiliar function to generate hands --
--      >> xs - a list of cards
--      >> n - number of hands to generate
--      >> p_cards - cards that already have been picked
--      >> c_acc - card accumulator to generate a hand when reaches 3 cards
--      >> h_acc - hand accumulator to return n hands
hand' :: [Card] -> Int -> [Card] -> [Card] -> [Hand] -> IO [Hand]
hand' xs n p_cards c_acc h_acc = do
    c <- pick xs
    if n == 0
        then return h_acc
    else if length c_acc == 3
        then hand' (remainingCards xs p_cards c) (n-1) p_cards [] (Hand (c_acc !! 0) (c_acc !! 1) (c_acc !! 2):h_acc)
    else
        hand' (remainingCards xs p_cards c) n (p_cards ++ c_acc) (c:c_acc) h_acc

-- Auxiliar function get a list of cards that have not been picked yet  --
--      >> xs - a list of cards
--      >> ys - a list with already picked cards
--      >> e - the most recent picked card that is not in the picked card list
remainingCards xs ys e = [x | x <- xs, x /= e, elem e ys == False]
