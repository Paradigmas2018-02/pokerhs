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


findThreeKind :: [Card] -> Maybe Hand
findThreeKind xs =
    if length xs <= 2 then Nothing else core xs []
    where core xs acc = do
            if length acc == 3 then Just (Hand {hvalue = 4, hcards = acc}) else if null xs then Nothing
            else if null acc then
                case findPair xs of 
                    Just pair -> core (remainingCards xs (acc ++ hcards pair)) (acc ++ hcards pair)
                    Nothing -> Nothing
            else
                case findByValue xs (head acc) of
                    Just card -> core xs (card:acc)
                    Nothing -> Nothing