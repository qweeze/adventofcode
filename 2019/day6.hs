import Data.List (minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map ((!))
import Data.Tuple (swap)
import Data.Ord (comparing)


ex1 = ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"]
ex2 = ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]

main :: IO ()
main = do
    content <- readFile "input.day6.txt"
    print $ solve2 content

parseRow :: String -> (String, String)
parseRow str = (x, y) where
    [x, y] = splitOn ")" str

mkMap :: [String] -> M.Map String String
mkMap list = M.fromList $ map (swap . parseRow) list

countOne :: M.Map String String -> String -> Int
countOne pairs key
    | M.member key pairs = 1 + countOne pairs (pairs ! key)
    | otherwise = 0

solve :: String -> Int
solve str = sum $ map (countOne pairs) (M.keys pairs)
    where
        pairs = mkMap $ lines str
    
--------- PART 2

solve2 str = answer where
    pairs = mkMap $ lines str

    pathBetween :: String -> String -> [String]
    pathBetween key1 key2
        | key1 == key2 = []
        | M.notMember key1 pairs = []
        | otherwise = nextKey : pathBetween nextKey key2
            where nextKey = pairs ! key1

    commonPoints = [p | p <- path1, p `elem` path2] where
        path1 = pathBetween "YOU" "COM"
        path2 = pathBetween "SAN" "COM"

    totalDistance :: String -> Int
    totalDistance p = length $ pathBetween "YOU" p ++ pathBetween "SAN" p

    closestPoint = minimumBy (comparing totalDistance) commonPoints

    answer = totalDistance closestPoint - 2