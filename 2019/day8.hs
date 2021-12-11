import Data.Char (digitToInt)
import Data.List (minimumBy, transpose)
import Data.Ord (comparing)

main :: IO ()
main = do
    content <- readFile "input.day8.txt"
    print $ solve2 content


solve :: String -> Int
solve str = calcAnswer layer
    where
        layer = minZeroslayer layers
        layers = toLayers (25 * 6) nums
        nums = map digitToInt $ init str


toLayers :: Int -> [Int] -> [[Int]]
toLayers _ [] = []
toLayers layerSize nums = let (layer, rest) = splitAt layerSize nums in
    layer : toLayers layerSize rest

minZeroslayer :: [[Int]] -> [Int]
minZeroslayer = minimumBy (comparing (length . filter (==0))) 

calcAnswer layer = count 1 layer * count 2 layer
    where
        count n = length . filter (==n)


-- Part 2

solve2 str = result 
    where
        result = toLayers 25 $ map getColor $ transpose layers
        layers = toLayers (25 * 6) nums
        nums = map digitToInt $ init str

getColor :: [Int] -> Int
getColor (2:xs) = getColor xs
getColor (x:xs) = x
