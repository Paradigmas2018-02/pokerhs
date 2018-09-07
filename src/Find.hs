module Find (
    findPair,
    findThreeKind,
) where

import Utils (findByValue, findBySuit)
import Deck (Card(..), Rank(..), Suit(..),remainingCards)
import Poker (Hand(..))

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
