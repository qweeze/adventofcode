main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ solve input


solve input = 
    -- head [x * y | x <- xs, y <- xs, x + y == 2020]
    head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
    where xs = map (read :: String -> Int) . words $ input
