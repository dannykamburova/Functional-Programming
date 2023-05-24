import Data.List
main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show)

getAreas :: Floating a => [Shape a] -> [a]
getAreas shapes = map area shapes

maxArea :: (Ord a, Floating a) => [Shape a] -> Shape a
maxArea shapes = foldl1 (\acc x -> if x > acc then x else acc) shapes

area :: Floating a => Shape a -> a
area (Circle r) = pi*r*r
area (Rectangle x y) = x*y
area (Triangle x y z) = sqrt (s*(s - x)*(s - y)*(s - z))
 where s = (x + y + z) / 2
area (Cylinder r h) = 2*pi*r*h + 2*pi*r*r

-- print $ simplify "1+2+x" == "x+3"
    --print $ simplify "x+2+x-2" == "2x"
    --print $ simplify "x+2-(x-2)" == "4"
    --print $ simplify "y+2+x-2" == "x+y"
    --print $ simplify "1+2+x+y+x+z+5-x-x-x+y" == "-x+2y+z+8"
    --print $ simplify "1+2+x+y+x-(x-x-x)+z+y-10" == "3x+2y+z-7"
   -- print $ simplify "1+2-(3-(3-2))-10" == "-9"