main :: IO ()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13
 
myGcdG :: Int -> Int -> Int
myGcdG x y 
 | x == 0 = y 
 | y == 0 = x
 | otherwise = myGcdG y (mod x y) 

myGcdPM :: Int -> Int -> Int
myGcdPM x y = helper x y
    where
    helper x 0 = x
    helper x y = helper y (mod x y)
