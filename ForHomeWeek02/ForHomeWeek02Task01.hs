main :: IO ()
main = do
    -- print $ countDigitsIter (-13) -- error "n was negative"
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3

    -- print $ countDigitsRec (-13) -- error "n was negative"
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3

countDigitsIter :: Int -> Int
countDigitsIter n 
 | n < 0 = error "x was negative"
 | otherwise = helper n
 where
    helper 0 = 0
    helper n = 1 + helper (div n 10)


countDigitsRec :: Int -> Int
countDigitsRec n
 | n < 0 = error "x was negative"
 | n == 0 = 0
 | otherwise = 1 + countDigitsRec (div n 10)
