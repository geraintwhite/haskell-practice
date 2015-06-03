import Data.List (sort)


type Party = String
type Ballot = [Party]


count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : (rmdups $ filter (/=x) xs)

frequency :: Eq a => [a] -> [(Int, a)]
frequency xs = [ (count x xs, x) | x <- (rmdups xs) ]

results :: [Party] -> [(Int, Party)]
results = sort . frequency

winner :: [Party] -> Party
winner = snd . last . results

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (\x -> length x > 0)

remove :: Eq a => a -> [[a]] -> [[a]]
remove x = map $ filter (/=x)

rank :: [Ballot] -> [Party]
rank = map snd . results . map head

election :: [Ballot] -> Party
election bs = case rank (rmempty bs) of
	[x]    -> x
	(x:xs) -> election (remove x bs)


votes :: [Party]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

ballots :: [Ballot]
ballots = [b1 , b2 , b3 , b4 , b5 , b6]
b1 = ["Blue", "Green"]
b2 = ["Green", "Blue", "Red"]
b3 = ["Blue"]
b4 = ["Red", "Green"]
b5 = ["Blue", "Red", "Green"]
b6 = ["Green", "Red"]

