import Data.List
main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [3, 4, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]


type Graph = [(Int, Int, Int)]

listLeaves :: Graph -> [Int]
listLeaves xs = nub $ filter (`notElem` parentElements) childElements
  where
    parentElements = map (\(x, _, _) -> x) xs
    childElements = concatMap (\(_, y, z) -> [y, z]) xs 


