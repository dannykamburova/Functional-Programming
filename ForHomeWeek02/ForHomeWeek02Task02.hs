main :: IO()
main = do
    -- print $ sumDigitsIter (-13) -- error "n was negative"
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6

sumDigitsIter :: Int -> Int
sumDigitsIter n = helper n 0
  where
    helper 0 res = res
    helper n res = helper (div n 10) (res + mod n 10)
