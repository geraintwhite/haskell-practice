quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]


mergesort'merge :: (Ord a) => [a] -> [a] -> [a]
mergesort'merge [] xs = xs
mergesort'merge xs [] = xs
mergesort'merge (x:xs) (y:ys)
    | x < y = x : mergesort'merge xs (y:ys)
    | otherwise = y : mergesort'merge (x:xs) ys

mergesort'split :: (Ord a) => [a] -> ([a], [a])
mergesort'split xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = mergesort'merge (mergesort ls) (mergesort rs)
    where (ls, rs) = mergesort'split xs


bubblesort'iter :: (Ord a) => [a] -> [a]
bubblesort'iter [x] = [x]
bubblesort'iter (x:y:xs)
  | x < y = x : bubblesort'iter (y:xs)
  | otherwise = y : bubblesort'iter (x:xs)

bubblesort' :: (Ord a) => [a] -> Int -> [a]
bubblesort' xs 0 = xs
bubblesort' xs i = bubblesort' (bubblesort'iter xs) (i - 1)

bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblesort' xs (length xs)
