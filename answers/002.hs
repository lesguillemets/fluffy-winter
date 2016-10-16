{-# LANGUAGE BangPatterns #-}
module Main where

main = print . sum . filter even . takeWhile (< 4000000) $ fib

fib :: [Int]
fib = f 1 1
    where
        f :: Int -> Int -> [Int]
        f !a !b = a : f b (a+b)
