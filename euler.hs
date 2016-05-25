-- https://projecteuler.net

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (reverse xs) == xs

-- Problem 1
problem1 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

-- Problem 2
fibo = 0 : 1 : zipWith (+) fibo (tail fibo)
problem2 = sum . takeWhile (< 4000000) . filter even $ fibo

-- Problem 3
n3 = 600851475143
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2..floor $ sqrt $ fromIntegral n]
problem3 = last $ primeFactors n3

-- Problem 4
problem4 = maximum [z | x <- [100..999], y <- [x..999], let z = x * y, let s = show z, s == reverse s]

-- Problem 5
problem5 = foldr1 lcm [1..20]

-- Problem 6
problem6 = (sum [1..100] **2) - (sum $ map (**2) [1..100])

-- Problem 7
isPrime n = null [x | x <- [2..floor $ sqrt $ fromIntegral n], n `mod` x == 0]
primes = filter isPrime [2..]
problem7 = primes !! 10000

-- Problem 8
n8 = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
problem8 = maximum [product (map (read . (:"")) (take 13 $ drop x $ show n8) :: [Int]) | x <- [0..(length (show n8) - 13)]]

-- Problem 9
n9 = 1000
problem9 = [ x*y*z | x <- [1..n9], y <- [x..n9], z <- [y..n9], x^2 + y^2 == z^2, x+y+z == n9 ]

-- Problem 10
problem10 = sum $ takeWhile (<2000000) primes
