{-

Quicksort

The quicksort algorithm for sorting a list of values
can be specified by the following two rules:

❚ The empty list is already sorted;


❚ Non-empty lists can be sorted by sorting the
tail values ≤ the head, sorting the tail values >
the head, and then appending the resulting
lists on either side of the head value.

-}


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
   qsort smaller ++ [x] ++ qsort larger
   where
      smaller = [a | a <- xs, a <= x]
      larger = [b | b <- xs, b > x]


{-

This is probably the simplest implementation of
quicksort in any programming language!

-}

{-
   Furthermore it can be written as only 3 lines.
   However it becomes less readable.
-}

qsort2 :: Ord a => [a] -> [a]
qsort2 [] = []
qsort2 (x:xs) = qsort2 [y | y <- xs, y <= x] ++ [x] ++ qsort2 [y | y <- xs, y > x]
