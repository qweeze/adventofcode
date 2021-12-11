import Data.List (maximumBy)
import Data.Ord (comparing)


input = ".#....#.###.........#..##.###.#.....##...\n\
        \...........##.......#.#...#...#..#....#..\n\
        \...#....##..##.......#..........###..#...\n\
        \....#....####......#..#.#........#.......\n\
        \...............##..#....#...##..#...#..#.\n\
        \..#....#....#..#.....#.#......#..#...#...\n\
        \.....#.#....#.#...##.........#...#.......\n\
        \#...##.#.#...#.......#....#........#.....\n\
        \....##........#....#..........#.......#..\n\
        \..##..........##.....#....#.........#....\n\
        \...#..##......#..#.#.#...#...............\n\
        \..#.##.........#...#.#.....#........#....\n\
        \#.#.#.#......#.#...##...#.........##....#\n\
        \.#....#..#.....#.#......##.##...#.......#\n\
        \..#..##.....#..#.........#...##.....#..#.\n\
        \##.#...#.#.#.#.#.#.........#..#...#.##...\n\
        \.#.....#......##..#.#..#....#....#####...\n\
        \........#...##...#.....#.......#....#.#.#\n\
        \#......#..#..#.#.#....##..#......###.....\n\
        \............#..#.#.#....#.....##..#......\n\
        \...#.#.....#..#.......#..#.#............#\n\
        \.#.#.....#..##.....#..#..............#...\n\
        \.#.#....##.....#......##..#...#......#...\n\
        \.......#..........#.###....#.#...##.#....\n\
        \.....##.#..#.....#.#.#......#...##..#.#..\n\
        \.#....#...#.#.#.......##.#.........#.#...\n\
        \##.........#............#.#......#....#..\n\
        \.#......#.............#.#......#.........\n\
        \.......#...##........#...##......#....#..\n\
        \#..#.....#.#...##.#.#......##...#.#..#...\n\
        \#....##...#.#........#..........##.......\n\
        \..#.#.....#.....###.#..#.........#......#\n\
        \......##.#...#.#..#..#.##..............#.\n\
        \.......##.#..#.#.............#..#.#......\n\
        \...#....##.##..#..#..#.....#...##.#......\n\
        \#....#..#.#....#...###...#.#.......#.....\n\
        \.#..#...#......##.#..#..#........#....#..\n\
        \..#.##.#...#......###.....#.#........##..\n\
        \#.##.###.........#...##.....#..#....#.#..\n\
        \..........#...#..##..#..##....#.........#\n\
        \..#..#....###..........##..#...#...#..#..\n"


type Point = (Int, Int)

mapToPoints :: String -> [Point]
mapToPoints input = 
    concat $
    zipWith mkCoords [0..] $
    map asteroidsPositions (lines input)

asteroidsPositions :: String -> [Int]
asteroidsPositions line =
    map fst $
    filter ((=='#') . snd) $
    zip [0..] line

mkCoords :: Int -> [Int] -> [Point]
mkCoords y row = zip row (repeat y)

minStep :: Point -> Point -> Point
minStep (x1, y1) (x2, y2) = 
    let
        (dx, dy) = (x2 - x1, y2 - y1)
        d = max 1 $ gcd dx dy
    in
        (dx `div` d, dy `div` d)

possibleIntersections' :: Point -> Point -> [Point]
possibleIntersections' p1 p2 
    | p1 == p2 = []
    | otherwise = let
        (x1, y1) = p1
        (dx, dy) = minStep p1 p2
        nextP = (x1 + dx, y1 + dy)
    in
        p1 : possibleIntersections' nextP p2 


safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs 

possibleIntersections :: Point -> Point -> [Point]
possibleIntersections p1 p2 = safeTail $ possibleIntersections' p1 p2

countVisible :: [Point] -> Point -> Int
countVisible points p =
    length $ filter (==[]) $ map (filter (`elem` points) . possibleIntersections p) points

points :: [Point]
points = mapToPoints input

bestPoint :: Point
bestPoint = maximumBy (comparing $ countVisible points) points

result :: Int
result = countVisible points bestPoint - 1
