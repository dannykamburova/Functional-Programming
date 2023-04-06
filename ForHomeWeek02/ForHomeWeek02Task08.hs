main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134

removeD :: Int -> Int -> Int
removeD d n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 res = res
    helper n res
     | n == 0 = res
     | d == mod n 10 = helper (div n 10) res
     | otherwise = rev (helper (div n 10) (10 * res + mod n 10))

rev :: Int -> Int
rev n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result * 10 + mod leftover 10)
