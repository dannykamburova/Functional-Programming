import Data.List
import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum (take n (filter (\ x -> elem (intToDigit d) (show x) && isPrime x ) [1 .. ]))

isPrime :: Int -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. n - 1], mod n d == 0]
