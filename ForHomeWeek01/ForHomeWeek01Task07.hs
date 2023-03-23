main :: IO ()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10

growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight = helper upSpeed downSpeed desiredHeight 1
    where
        helper :: Int -> Int -> Int -> Int -> Int
        helper upSpeed downSpeed 0 result = result
        helper upSpeed downSpeed leftover result = helper upSpeed downSpeed (desiredHeight - upSpeed + downSpeed) (result + 1)
        
