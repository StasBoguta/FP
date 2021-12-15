{-# OPTIONS_GHC -Wall #-}
module Bohuta02 where
    lst::[Integer]
    lst = [5,7,9,2]

    sumFr :: [Integer] -> Integer
    sumFr = foldr (+) 0

    factorial :: Integer -> Integer
    factorial x = foldl (*) 1 [1..x]

    concatFr :: [Integer] -> [Integer] -> [Integer]
    concatFr xs ys = foldr (:) ys xs

    insert:: [Integer] -> Integer -> [Integer]
    insert xs v = (++) (filter (v>) xs) (v : filter (v<=) xs)

    sortInsert :: [Integer] -> [Integer]
    sortInsert = foldl insert []

    map2 :: (a->b->c) -> [a] -> [b] -> [c]
    map2 f xs ys = [f ((!!) xs i) ((!!) ys i) | i <- [0..min (length xs) (length ys)-1]]

    toDouble :: Integer -> Double
    toDouble m = if m == 0 then 0 else 1 + toDouble (m-1)

    expPart :: Integer -> Integer -> Double
    expPart m n = sum [toDouble m^i/toDouble(factorial i)| i <- [1..n]]

    triangle :: [Integer]
    triangle = scanl1 (+) [1..]

    piramid :: [Integer]
    piramid = scanl1 (+) [i*i | i <- [1..]]

    helpIndexes :: Int -> [Int] -> [[Int]]
    helpIndexes xs ys = [drop (i-xs) (take i ys)| i <- [xs .. length ys]]

    createIndex:: [Int] -> [[Int]] -> Int -> [Int]
    createIndex xs ys x
      | null ys = []
      | head ys == xs = x:createIndex xs (tail ys) (x+1)
      | otherwise = createIndex xs (tail ys) (x+1)

    indexes :: [Int] -> [Int] -> [Int]
    indexes xs ys = createIndex xs (helpIndexes (length xs) ys) 0

