main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11


isPalindrome :: Int -> Bool
isPalindrome n = rev n == n

rev :: Int -> Int
rev n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result = helper (div leftover 10) (result * 10 + mod leftover 10)

countPalindromes :: Int -> Int -> Int
countPalindromes x y
 | x > y = countPalindromes y x
 | otherwise = helper (x + 1) 0
 where
    helper n counter
     | n >= y = counter
     | isPalindrome n = helper (n + 1) (counter + 1)
     | otherwise = helper (n + 1) counter

