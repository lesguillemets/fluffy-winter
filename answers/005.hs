primes :: [Int]
primes = [2,3,5,7,11,13,17,19] -- I know

maxPower :: Int -> Int -> Int
maxPower maxim a = floor $ logBase (fromIntegral a) (fromIntegral maxim)

solve :: Int -> Int
solve n = product . zipWith (^) primes $ map (maxPower n) primes

main = print $ solve 20

