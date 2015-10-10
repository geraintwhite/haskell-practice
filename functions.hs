import Data.Char

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
	| even n = n : collatz (div n 2)
	| odd n  = n : collatz (n*3 + 1)

intersperse' :: a -> [a] -> [a]
intersperse' _ [x] = [x]
intersperse' s (x:xs) = x : s : (intersperse' s xs)

intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [x] = x
intercalate' s (x:xs) = x ++ s ++ (intercalate' s xs)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs ]

encode :: Int -> String -> String
encode n = map chr . map (+n) . map ord

decode :: Int -> String -> String
decode n = encode $ negate n

fibo = 0 : 1 : zipWith (+) fibo (tail fibo)

factorial :: Int -> Int
factorial x
    | x == 1    = x
    | otherwise = x * factorial (x-1)

isPrime :: Integral a => a -> Bool
isPrime n = null [ x | x <- [2..floor $ sqrt $ fromIntegral n], n `mod` x == 0 ]

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]
