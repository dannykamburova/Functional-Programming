main :: IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6

mySumRecNonPM :: [Int] -> Int
mySumRecNonPM xs = helper xs 0
 where
    helper xs res
     | null xs = res
     | otherwise = helper (tail xs) (res + head xs)

mySumRecPM :: [Int] -> Int
mySumRecPM [] = 0
mySumRecPM (x:xs) = x + mySumRecPM xs

mySumFunc :: [Int] -> Int
mySumFunc = sum