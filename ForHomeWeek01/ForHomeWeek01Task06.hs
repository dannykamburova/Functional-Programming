main :: IO ()
main = do
    print $ canCarry 5 15 3 == "Yes"
    print $ canCarry 1 5 4 == "Yes"
    print $ canCarry 13 25 2 == "No"
    print $ canCarry 24 104.44 21.12 == "No"
    print $ canCarry 51 34.75 19.852 == "No"
    print $ canCarry 42 95.11 0.51 == "Yes"
    
canCarry :: Int -> Double -> Double -> [Char]
canCarry x y z
 | x < 0 = error "The number of products was negative"
 | y < 0 = error "John's hosting capacity was negative"
 | z < 0 = error "The weight of a product was negative"
 | (fromIntegral x) * z <= y = "Yes"
 | otherwise = "No"