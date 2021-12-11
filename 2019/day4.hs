import Data.List (sort)

isValid :: Int -> Bool
isValid num = hasIncreasingDigits n && hasDoubleSameDigits n
    where n = show num

hasIncreasingDigits :: String -> Bool
hasIncreasingDigits s = sort s == s

hasDoubleSameDigits :: String -> Bool
hasDoubleSameDigits [] = False
hasDoubleSameDigits [_] = False
hasDoubleSameDigits (c1:c2:cs) = c1 == c2 || hasDoubleSameDigits (c2:cs)

hasOnlyDoubleSameDigits :: String -> Bool
hasOnlyDoubleSameDigits s = check2 s (length s)

idx xs pos
    | pos < 0 = Nothing
    | pos >= length xs = Nothing
    | otherwise = Just (xs !! pos)

check2 s pos
    | pos < 0 = False
    | otherwise = (
        idx s (pos - 1) /= idx s pos && 
        idx s pos == idx s (pos + 1) &&
        idx s (pos + 1) /= idx s (pos + 2)
    ) || check2 s (pos - 1)


isValid2 :: Int -> Bool
isValid2 num = hasIncreasingDigits n && hasOnlyDoubleSameDigits n
    where n = show num

result = length $ filter isValid [153517..630395]
result2 = length $ filter isValid2 [153517..630395]
