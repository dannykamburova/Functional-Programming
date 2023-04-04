main :: IO()
main = do
  print $ eqSumPowDig 100 2 == 0
  print $ eqSumPowDig 1000 2 == 0
  print $ eqSumPowDig 2000 2 == 0
  print $ eqSumPowDig 200 3 == 153
  print $ eqSumPowDig 370 3 == 523
  print $ eqSumPowDig 370 3 == 523
  print $ eqSumPowDig 400 3 == 894
  print $ eqSumPowDig 500 3 == 1301
  print $ eqSumPowDig 1000 3 == 1301
  print $ eqSumPowDig 1500 3 == 1301

isSpecial :: Int -> Int -> Bool
isSpecial n pow = n == helper n 0
 where 
   helper 0 result = result
   helper n result = helper (div n 10) ((mod n 10)^pow + result)

eqSumPowDig :: Int -> Int -> Int
eqSumPowDig hMax power
  | hMax < 1 = error "The argument 'hMax' should be bigger than 1!"
  | power < 2 = error "The argument 'power' should be bigger than 2!"
  | otherwise = helper2 2 0
 where
   helper2 :: Int -> Int -> Int
   helper2 n res
     | n > hMax = res
     | isSpecial n power = helper2 (n + 1) (n + res)
     | otherwise = helper2 (n + 1) res

