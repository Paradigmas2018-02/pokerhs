module Utils(
    createTuples,
) where


-- Create a list of tuples with the given list --
-- by combining two elements at time, the list must have
-- a even number of elements e.g [1,2,3,4] -> [(1,2), (3,4)]
createTuples :: [a] -> [(a,a)]
createTuples [] = []
createTuples (x:y:t) = (x, y) : createTuples t