module Find (
    findPair
) where

findPair :: [Card] -> Maybe Hand
findPair list
    | null list = Nothing
    | length list == 1 = Nothing
    | otherwise = 
        case findByValue (tail list) (head list) of
            Just card -> Just (Hand {hvalue=2, hcards=[head list, card]}) 
            Nothing -> findPair (tail list)