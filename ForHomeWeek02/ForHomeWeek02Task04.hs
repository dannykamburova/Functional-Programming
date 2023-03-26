main :: IO()
main = do
    print $ countOccurences 121 1 == 2
    print $ countOccurences 222 1 == 0
    print $ countOccurences 100 0 == 2
    print $ countOccurences 0 0 == 1

countOccurences :: Int -> Int -> Int
countOccurences 0 0 = 1
countOccurences 0 _ = 0
countOccurences n k
 | k == (mod n 10) = 1 + countOccurences (div n 10) k
 | otherwise = countOccurences (div n 10) k
