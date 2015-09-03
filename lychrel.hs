isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (reverse xs) == xs

lychrel :: Int -> Int
lychrel x
  | isPalindrome $ show x = x
  | otherwise = lychrel $ (read $ reverse $ show x) + x
