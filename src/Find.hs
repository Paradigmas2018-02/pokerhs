module Find (
    findPair,
    findThreeKind,
) where

import Utils (findByValue)
import Deck (Card(..), Rank(..), remainingCards)
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


findSequence :: [Card] -> Card -> Maybe [Card]
findSequence xs x =
    let core xs x acc 
            | length acc == 5 = Just acc
            | otherwise =
                case findByValue xs x of
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
