module Utils(
    createTuples,
    destroyTuples,
) where


-- Create a list of tuples with the given list --
-- by combining two elements at time, the list must have
-- a even number of elements e.g [1,2,3,4] -> [(1,2), (3,4)]
createTuples :: [a] -> [(a,a)]
createTuples [] = []
createTuples (x:y:t) = (x, y) : createTuples t


-- Convert a list of tuples to a list of the tuple elements --
-- e.g [(1,2), (3,4)] -> [1,2,3,4]
destroyTuples :: [(a,a)] -> [a]
destroyTuples [] = []
destroyTuples xs = concat [destroyTuple x | x <- xs]


-- Convert a a tuple to a list --
-- e.g (1,2) -> [1,2]
destroyTuple :: (a,a) -> [a]
destroyTuple (x, y) = [x, y]