main :: IO()
main = do 
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98

findSum :: Int -> Int -> Int -> Int
findSum a b n = helper 0 0 0 0
  where
    helper s0 s1 s2 count
      | count == n = b * a + s0 + s1 + s2
      | otherwise = helper s1 s2 (s2 + 2^count * b) (count + 1)