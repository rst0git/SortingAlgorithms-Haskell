-- Returns first half of a list
firsthalf :: [a] -> [a]
firsthalf x = take (length x `div` 2) x

-- Returns second half of a list
secondhalf :: [a] -> [a]
secondhalf x = drop (length x `div` 2) x


{-

Merges two sorted lists of values to give
a single sorted list.

For example:

> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]

-}
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
          | (x <= y)  = x : merge xs (y:ys)
          | otherwise = y : merge (x:xs) ys

{-

Recursive function that implements merge sort, which can be
specified by the following two rules:

❚ Lists of length ≤ 1 are already sorted;

❚ Other lists can be sorted by sorting the two
halves and merging the resulting lists.

-}

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort x = merge (msort (firsthalf x)) (msort (secondhalf x))
