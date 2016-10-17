solve :: Int -> Int
solve n = ((n*(n+1))^2) `div` 4 -  n*(n+1)*(2*n+1) `div` 6

main = print $ solve 100
