fact :: Int -> Int
fact x
    | x == 0 = 1
    | otherwise = x * fact (x-1)

main = print (fact 26)
