main :: IO ()
main = do
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1

snail :: Int -> Int -> Int -> Int
snail heightCol distDay distNight
 | heightCol <= 0 = 0
 | heightCol <= distDay = 1
 | otherwise = 1 + snail (heightCol - distDay + distNight) distDay distNight
