reduce ::  -> [d] -> n -> r
reduce func lst n
    | length(lst) == 1 = n
    | otherwise = reduce func (tail lst) (func lst[0] n)


add :: Int -> Int -> Int
add x y = x + y


main = print (reduce add [1,2,3])