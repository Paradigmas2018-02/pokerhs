module Find (
    findPair,
    findThreeKind,
) where

import Utils (findByValue)
import Deck (Card, remainingCards)
import Poker (Hand(..))

findPair :: [Card] -> Maybe Hand
findPair list
    | null list = Nothing
    | length list == 1 = Nothing
    | otherwise = 
        case findByValue (tail list) (head list) of
            Just card -> Just (Hand {hvalue=2, hcards=[head list, card]}) 
            Nothing -> findPair (tail list)

            
-- Find three cards with the same value (kind) --
--  >> xs - a list of cards
findThreeKind :: [Card] -> Maybe Hand
findThreeKind xs =
    let core xs acc 
            | length acc == 3 = Just (Hand {hvalue = 4, hcards = acc})
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
            | length acc == 4 = Just (Hand {hvalue = 3, hcards = acc})
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