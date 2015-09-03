isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (reverse xs) == xs

lychrel :: Int -> Int
lychrel 0 = 0
lychrel n = f n
  where f x
          | isPalindrome $ show x = x
          | otherwise = f $ (read $ reverse $ show x) + x
