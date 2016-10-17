import Data.List (maximum, uncons, foldl')
import Data.Maybe (fromMaybe)
import System.CPUTime (getCPUTime)
isPalindrome :: Int -> Bool
isPalindrome n = let s = show n in s == reverse s

divisibleBy :: Int -> Int -> Bool
divisibleBy n p = n `mod` p == 0
safeHead :: [a] -> Maybe a
safeHead = fmap fst . uncons

findPGreater :: Int -> Int -> Maybe Int
findPGreater cand n
    = safeHead . filter isPalindrome . takeWhile (>= cand)
        . map (*n) $ cands
    where
        cands = case n `mod` 11 of
                     0 -> [n, n-1 .. 100]
                     k -> [n-k, n-k-11 .. 100]


ans = foldl' (\ cand n -> fromMaybe cand $ findPGreater cand n) 0
        [999, 998 .. 100]

main = do
    x <- getCPUTime
    print ans
    y <- getCPUTime
    print $ y - x
    x' <- getCPUTime
    print . maximum . filter isPalindrome $ [
        x*y | x <- [999,998 .. 100], y <- [x, x-1 .. 100]
        ]
    y' <- getCPUTime
    print $ y' - x'
