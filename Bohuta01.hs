{-# OPTIONS_GHC -Wall #-}
module Bohuta01 where

    factorial :: Integer -> Integer
    factorial x = if x == 0 then 1 else x * factorial (x-1)

    listSum :: [Int] -> [Int] -> [Int]
    listSum xs ys
      | null xs = if null ys then [] else head ys : listSum xs (tail ys)
      | null ys = head xs : listSum (tail xs) ys
      | otherwise = (head xs + head ys) : listSum (tail xs) (tail ys)

    oddEven :: [Int] -> [Int]
    oddEven xs
      | null xs = []
      | null (tail xs) = head xs : oddEven (tail xs)
      | otherwise = head (tail xs):(head xs : oddEven(tail (tail xs)))

    position ::  Int -> [Int] -> Int
    position n xs
      | null xs = -1
      | head xs == n = 0
      | position n (tail xs) /= -1 = 1 + position n (tail xs)
      | otherwise = -1

    set :: [Int] -> [Int]
    set xs
      | null xs = []
      | position (head xs) (tail xs) == -1 = head xs : set (tail xs)
      | otherwise = set(tail  xs)

    helpunion :: [Int] -> [Int] -> [Int]
    helpunion xs ys
      | null xs = ys
      | position (head xs) ys == -1 = helpunion (tail xs) (head xs :ys)
      | otherwise = helpunion (tail xs) ys

    union :: [Int] -> [Int] -> [Int]
    union xs ys = helpunion xs (set ys)

    helpintersection :: [Int] -> [Int] -> [Int]
    helpintersection xs ys
      | null xs = []
      | position (head xs) ys /= -1 = head xs : helpintersection (tail xs) ys
      | otherwise = helpintersection (tail xs) ys

    intersection :: [Int] -> [Int] -> [Int]
    intersection xs ys = set (helpintersection xs ys)

    createFactorial :: Integer -> [Integer]
    createFactorial n = factorial n : createFactorial (n+1)

    factorialsM :: [Integer]
    factorialsM = createFactorial 1