module Find where

import Deck (Card(..), Rank(..), Suit(..),remainingCards)
import Utils (findByValue, findBySuit, findCard)
import Poker (Hand(..))
import Data.Maybe (fromMaybe)

-- Constant values for hands in poker
-- Used to compare poker hands ranks.
valueHighCard = Rank{rvalue = 1, rname = "HighCard"}
valuePair = Rank{rvalue = 2, rname = "Pair"}
valueTwoPair = Rank{rvalue = 3, rname = "Two Pairs"}
valueThreeKind = Rank{rvalue = 4, rname = "Three of a Kind"}
valueStraight = Rank{rvalue = 5, rname = "Straight"}
valueFlush = Rank{rvalue = 6, rname = "Flush"}
valueFullHouse = Rank{rvalue = 7, rname = "Full House"}
valueFourKind = Rank{rvalue = 8, rname = "Four of a Kind"}
valueStraightFlush = Rank{rvalue = 9, rname = "Straight Flush"}
valueRoyalFlush = Rank{rvalue = 10, rname = "Royal Straight Flush"}


-- All functions for finding hands on poker --
findFunctions = [findRoyalFlush, findStraightFlush, findFourKind,
                findFullHouse, findFlush, findStraight, findThreeKind,
                findTwoPairs, findPair, findHighCard]


-- Return the best hand possible of a list of cards --
findHand :: [Card] -> Hand
findHand xs =
    let core fs xs = fromMaybe (core (tail fs) xs) (head fs xs)
    in core findFunctions xs


-- High card is the lowest rank in poker, in a list of cards this should find the higher value card.
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


-- Find a pair of cards on a list of cards.
-- E.G [4c, 5s, 8c, 4s, 9c] this list should return a hand witht the pair with 4 clubs and 4 spades. 
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


-- Find a sequence of 5 cards only by value
-- E.G [4 clubs, 5 hearts, 6 clubs, 7 spades, 8 diamonds]
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


-- Find four cards of the same kind, looking for three of a kind and another card.
-- E.G [2 clubs, 2 hearts, 2 spades, 2 diamonds]
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


-- Find a Sequence of 5 cards with the same suit.
-- E.G [2C, 3C, 4C, 5C, 6C]
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


-- Find a Royal Straight Flush
-- Made up of a sequence of the cards 10, 11, J, Q, K and A with the same suit.
findRoyalFlush :: [Card] -> Maybe Hand
findRoyalFlush xs =
    case findStraightFlush xs of
        Just hand -> if contains [rvalue $ rank x | x <- hcards hand] [10, 11, 12, 13, 14]
            then
                Just (Hand {hvalue = valueRoyalFlush, hcards = hcards hand})
            else
                Nothing
        Nothing -> Nothing 


-- Find if a card has a complete sequence on the list
-- E.G. In this list of cards [4, K, 6, 8, 7, 5, 9]
-- 4 has a sequence of [4, 5, 6, 7, 8]
-- 5 has a sequence of [5, 6, 7, 8, 9]
-- No other cards form a complete sequence
findSequence :: [Card] -> Card -> Maybe [Card]
findSequence xs x =
    let core xs x acc 
            | length acc == 5 = Just acc
            | otherwise =
                case findByValue xs x of
                    Just card -> core xs Card {rank = Rank {rvalue = nextCard card acc, rname = rname $ rank card}, suit = suit card} (card:acc)
                    Nothing -> Just acc
    
    in core xs x []


-- Find if a card has a complete sequence with the same suit on the list
-- E.G. In this list of cards [4 c, 5 c , 6 c , 7 c, 8 c], 4c has a complete sequence of 5 cards with the same suit;
-- E.G In this list of cards [4 c, 5 c, 6 h, 7 s, 8 c], it has a sequence but no cards have a sequence with 5 cards in the same suit;
-- In these examples "c" means "clubs", "h" "hearts" and s "spades".
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
