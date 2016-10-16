
main = print $ f 3 + f 5 - f 15
    where
        f = (`sumMuliplesUpto` 999)

sumMuliplesUpto :: Int -> Int -> Int
sumMuliplesUpto n upto =
    let m = upto `div` n
        in (n + n*m) * m `div` 2
