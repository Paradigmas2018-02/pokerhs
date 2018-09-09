module Find (
    findPair,
    findThreeKind,
) where

import Deck (Card(..), Rank(..), Suit(..),remainingCards)
import Utils (findByValue, findBySuit, findCard)
import Poker (Hand(..))
import Data.Maybe (fromMaybe)

valueHighCard = 1
valuePair = 2
valueTwoPair = 3
valueThreeKind = 4
valueStraight = 5
valueFlush = 6
valueFullHouse = 7
valueFourKind = 8
valueStraightFlush = 9
valueRoyalFlush = 10


-- All find hand functions --
findFunctions = [findRoyalFlush, findStraightFlush, findFourKind,
                findFullHouse, findFlush, findStraight, findThreeKind,
                findTwoPairs, findPair, findHighCard]


-- Return the best hand possible of a list of cards --
findHand :: [Card] -> Hand
findHand xs =
    let core fs xs = fromMaybe (core (tail fs) xs) (head fs xs)
    in core findFunctions xs


findHighCard :: [Card] -> Maybe Hand
findHighCard xs =
    let core xs h_card
            | null xs = Just Hand{hvalue = valueHighCard, hcards = [h_card]}
            | otherwise = 
                if rvalue (rank (head xs)) > rvalue (rank h_card)
                    then
                        core (tail xs) (head xs)
                else
                    core (tail xs) h_card

    in core xs (head xs)


findPair :: [Card] -> Maybe Hand
findPair list
    | null list = Nothing
    | length list == 1 = Nothing
    | otherwise = 
        case findByValue (tail list) (head list) of
            Just card -> Just (Hand {hvalue=valuePair, hcards=[head list, card]}) 
            Nothing -> findPair (tail list)

            
-- Find three cards with the same value (kind) --
--  >> xs - a list of cards
findThreeKind :: [Card] -> Maybe Hand
findThreeKind xs =
    let core xs acc 
            | length acc == 3 = Just (Hand {hvalue = valueThreeKind, hcards = acc})
            | null xs = Nothing
            | null acc =
                case findPair xs of 
                    Just pair -> core (remainingCards xs (acc ++ hcards pair)) (acc ++ hcards pair)
                    Nothing -> Nothing
            | otherwise =
                case findByValue xs (head acc) of
                    Just card -> core xs (card:acc)
                    Nothing -> Nothing

    in if length xs <= 3 then Nothing else core xs []

    
-- Find two pairs of cards with the same value --
--  >> xs - a list of cards
findTwoPairs :: [Card] -> Maybe Hand
findTwoPairs xs =
    let core xs acc
            | length acc == 4 = Just (Hand {hvalue = valueTwoPair, hcards = acc})
            | null xs = Nothing
            | null acc =
                case findPair xs of 
                    Just pair -> core (remainingCards xs (acc ++ hcards pair)) (acc ++ hcards pair)
                    Nothing -> Nothing
            | otherwise = 
                case findPair xs of
                    Just hand -> core xs (hcards hand ++ acc)
                    Nothing -> Nothing
    
    in if length xs <= 3 then Nothing else core xs []


-- Find a Full house hand, made of Three of a kind and a pair
findFullHouse :: [Card] -> Maybe Hand
findFullHouse xs =
    let core xs acc
            | length acc == 5 = Just (Hand {hvalue = valueFullHouse, hcards = acc}) 
            | null xs = Nothing
            | null acc =
                -- Three of a kind must be found first or it will be matched as a pair.
                case findThreeKind xs of 
                    Just hand -> core (remainingCards xs (acc ++ hcards hand)) (acc ++ hcards hand)
                    Nothing -> Nothing
            | otherwise =
                case findPair xs of
                    Just hand -> core xs (hcards hand ++ acc)
                    Nothing -> Nothing

    -- Less than 4 cards can't be a Full House
    in if length xs <= 4 then Nothing else core xs []

    
-- Find a Flush, five cards of the same suit
findFlush :: [Card] -> Maybe Hand
findFlush xs =
    let core xs acc1 acc2 acc3 acc4
            | length acc1 >= 5 = Just (Hand {hvalue = valueFlush, hcards = acc1})
            | length acc2 >= 5 = Just (Hand {hvalue = valueFlush, hcards = acc2})
            | length acc3 >= 5 = Just (Hand {hvalue = valueFlush, hcards = acc3})
            | length acc4 >= 5 = Just (Hand {hvalue = valueFlush, hcards = acc4})
            | null xs = Nothing
            | otherwise = 
                case svalue (suit (head xs)) of
                    1 -> core (tail xs) (head xs : acc1) acc2 acc3 acc4
                    2 -> core (tail xs) acc1 (head xs : acc2) acc3 acc4
                    3 -> core (tail xs) acc1 acc2 (head xs : acc3) acc4
                    4 -> core (tail xs) acc1 acc2 acc3 (head xs : acc4)
                    _ -> Nothing
    in if length xs <= 4 then Nothing else core xs [] [] [] []


findStraight :: [Card] -> Maybe Hand
findStraight xs =
    let core xs acc ys
            | length acc == 5 = Just (Hand {hvalue = valueStraight, hcards = acc})
            | null xs = Nothing
            | otherwise =
                case findSequence ys (head xs) of
                    Just seq -> core (tail xs) seq ys
                    Nothing -> core (tail xs) [] ys

    in if length xs <= 4 then Nothing else core xs [] xs


findFourKind :: [Card] -> Maybe Hand
findFourKind xs = 
    let core xs acc
            | length acc == 5 = Just (Hand {hvalue = valueFourKind, hcards = acc})
            | null xs = Nothing
            | null acc =
                case findThreeKind xs of
                    Just hand -> core (remainingCards xs (hcards hand)) (hcards hand ++ acc)
                    Nothing -> Nothing
            | otherwise = 
                case findByValue xs (head acc) of
                    Just card -> Just (Hand {hvalue = valueFourKind, hcards = card:acc})
                    Nothing -> Nothing

    in core xs []


findStraightFlush :: [Card] -> Maybe Hand
findStraightFlush xs =
    let core xs acc ys
            | length acc == 5 = Just (Hand {hvalue = valueStraightFlush, hcards = acc})
            | null xs = Nothing
            | otherwise =
                case findSequenceWithSuit ys (head xs) of
                    Just seq -> core (tail xs) seq ys
                    Nothing -> core (tail xs) [] ys

    in if length xs <= 4 then Nothing else core xs [] xs


findRoyalFlush :: [Card] -> Maybe Hand
findRoyalFlush xs =
    case findStraightFlush xs of
        Just hand -> if contains [rvalue $ rank x | x <- hcards hand] [10, 11, 12, 13, 14]
            then
                Just (Hand {hvalue = valueRoyalFlush, hcards = hcards hand})
            else
                Nothing
        Nothing -> Nothing 


findSequence :: [Card] -> Card -> Maybe [Card]
findSequence xs x =
    let core xs x acc 
            | length acc == 5 = Just acc
            | otherwise =
                case findByValue xs x of
                    Just card -> core xs Card {rank = Rank {rvalue = nextCard card acc, rname = rname $ rank card}, suit = suit card} (card:acc)
                    Nothing -> Just acc
    
    in core xs x []


findSequenceWithSuit :: [Card] -> Card -> Maybe [Card]
findSequenceWithSuit xs x =
    let core xs x acc
            | length acc == 5 = Just acc
            | otherwise = 
                case findCard xs x of
                    Just card -> core xs Card {rank = Rank {rvalue = nextCard card acc, rname = rname $ rank card}, suit = suit card} (card:acc)
                    Nothing -> Just acc
                    
    in core xs x []


-- Return the next card following this sequence --
-- [A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, .. A]
nextCard :: Card -> [Card] -> Int
nextCard c xs
    -- The next card after A if there is a K is none. The can't be a sequence like: Q, K, A, 2, 3.
    | 13 `elem` [rvalue $ rank x | x <- xs] && rvalue (rank c) == 14 = 15
    | rvalue (rank c) >= 14 = 2
    | otherwise = rvalue (rank c) + 1


-- Check if a list contains all values of another --
--  >> xs - list that must contains
--  >> ys - list with values contained on xs
contains :: Eq a => [a] -> [a] -> Bool
contains xs ys  
    | length [x | x <- xs, x `elem` ys] == length ys = True
    | otherwise = False
