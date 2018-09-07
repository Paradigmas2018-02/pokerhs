module Utils(
    createTuples,
    destroyTuples,
    development,
    findCard
) where

import Deck(Rank(..), Card(..), Suit(..))

-- Convert a list of card tuples to a list of cards --
--      >> xs - a list of card tuples
splitCardTuples :: IO [(Card, Card)] -> IO [Card]
splitCardTuples xs = destroyTuples <$> xs

-- Create a list of tuples with the given list --
-- by combining two elements at time, the list must have
-- a even number of elements e.g [1,2,3,4] -> [(1,2), (3,4)]
createTuples :: [a] -> [(a,a)]
createTuples [] = []
createTuples (x:y:t) = (x, y) : createTuples t


-- Convert a list of tuples to a list of the tuple elements --
-- e.g [(1,2), (3,4)] -> [1,2,3,4]
destroyTuples :: [(a,a)] -> [a]
destroyTuples [] = []
destroyTuples xs = concat [destroyTuple x | x <- xs]


-- Convert a a tuple to a list --
-- e.g (1,2) -> [1,2]
destroyTuple :: (a,a) -> [a]
destroyTuple (x, y) = [x, y]

development :: IO()
development = putStrLn "Essa feature está sendo desenvolvida. Por favor aguarde novas atualizações do sistema";

-- Find a card with the same value on the list
findByValue :: [Card] -> Card -> Maybe Card
findByValue list card 
    | null list = Nothing
    | rvalue (rank (head list)) == rvalue (rank card) = Just (head list)
    | otherwise = findByValue (tail list) card


-- Find a card with the same suit on the list
findBySuit :: [Card] -> Card -> Maybe Card
findBySuit list card 
    | null list = Nothing
    | svalue (suit (head list)) == svalue (suit card) = Just (head list)
    | otherwise = findBySuit (tail list) card


-- Find a card with the same value and suit on the list
findCard :: [Card] -> Card -> Maybe Card
findCard list card 
    | null list = Nothing
    | (svalue (suit (head list)) == svalue (suit card)) && (rvalue (rank (head list)) == rvalue (rank card))  = Just (head list)
    | otherwise = findCard (tail list) card