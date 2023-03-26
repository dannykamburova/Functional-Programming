main :: IO()
main = do
    -- print $ sumDigitsIter (-13) -- error "n was negative"
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6

sumDigitsIter :: Int -> Int
sumDigitsIter n 
 | n < 0 = error "n was negative"
 | otherwise = helper n 
 where
    helper 0 = 0
    helper n = (mod n 10) + helper (div n 10)