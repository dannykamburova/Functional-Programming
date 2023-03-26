main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 

sumDigitsRec :: Int -> Int
sumDigitsRec 0 = 0
sumDigitsRec num = sumDigitsRec (div num 10) + (mod num 10)

isInteresting :: Int -> Bool
isInteresting n = mod n (sumDigitsRec n) /= n