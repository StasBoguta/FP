{-# OPTIONS_GHC -Wall #-}

module Bohuta07 where

    type PolinomOne = [(Int,Rational)]
    type Linear   = [Row]
    type Row      = [Rational]
    data Solution = Empty | One Row  | Many [PolinomOne]
                    deriving (Show, Eq)

    -- Задача 1.a -----------------------------------------
    coef :: Rational -> PolinomOne -> PolinomOne
    coef c p
        | c == 0 = []
        | otherwise = [(fst i, snd i * c)|i<-p]

    -- Задача 1.b -----------------------------------------
    add :: PolinomOne -> PolinomOne -> PolinomOne
    add p1 p2 = filter isNotNull (sortInsert (sumTwoPoly p1 p2) p2)

    isNotNull :: (Int, Rational) -> Bool
    isNotNull r = snd r /= 0

    sumTwoPoly:: PolinomOne -> PolinomOne -> PolinomOne
    sumTwoPoly p1 p2 = [(fst i, if null (filterArr (fst i) p2) then snd i else snd i + snd(head (filterArr (fst i) p2)))| i<- p1]

    isInPoly:: (Int, Rational) -> PolinomOne -> Bool
    isInPoly r p = filterArr (fst r) p /= []

    addTwoLinear:: (Int, Rational) -> (Int, Rational) -> (Int, Rational)
    addTwoLinear p1 p2 = (fst p1, snd p2 + snd p1)

    filterArr:: Int -> PolinomOne -> PolinomOne
    filterArr i = filter (isMathPosition i)

    isMathPosition::  Int -> (Int, Rational) -> Bool
    isMathPosition i r = fst r == i

    sortInsert :: PolinomOne -> PolinomOne -> PolinomOne
    sortInsert = foldr insert

    isBig :: Int -> (Int, Rational) -> Bool
    isBig i r = i > fst r

    isLess :: Int -> (Int, Rational) -> Bool
    isLess i r = i < fst r

    insert:: (Int,Rational) -> PolinomOne -> PolinomOne
    insert v xs = if isInPoly v xs then xs else (++) (filter (isBig (fst v)) xs) (v:filter (isLess (fst v)) xs)
    -- Задача 1.c -----------------------------------------
    unify :: PolinomOne -> PolinomOne
    unify p = filter isNotNull (foldr addOne [] p)

    addOne::(Int,Rational) -> PolinomOne -> PolinomOne
    addOne r p = if isInPoly r p then [(fst i, if fst i == fst r then snd r + snd i else snd i)|i <- p] else (++) (filter (isBig (fst r)) p) (r:filter (isLess (fst r)) p)
    -- Задача 2.a -----------------------------------------
    findFree :: [PolinomOne] -> [Int]
    findFree ps = [fst (head p)|p <- ps, length p == 1 && fst (head p) /= 0]

    -- Задача 2.b -----------------------------------------
    iswfCommon ::  [PolinomOne]  -> Bool
    iswfCommon ps = and [iswfCheck p (findFree ps)| p <- ps]

    iswfCheck :: PolinomOne -> [Int] -> Bool
    iswfCheck p index = and [fst r `elem` index| r <- p, fst r /= 0]

    -- Задача 3.a -----------------------------------------
    isSimple :: Linear -> Bool
    isSimple s = not (null s) && and [  length r == 1| r <- s]

    -- Задача 3.b -----------------------------------------
    solveSimple :: Linear -> Maybe [PolinomOne]
    solveSimple l = if and [length r == 1 && head r == 0| r <- l] then Just [] else Nothing

    -- Задача 4.a -----------------------------------------
    findRow :: Linear -> Maybe Int
    findRow l = if and [head r == 0|r <- l] then Nothing else Just (head [i+1|i <- [0..length l - 1], head (l!!i) /= 0])

    -- Задача 4.b -----------------------------------------
    exchangeRow :: [a] -> Int -> [a]
    exchangeRow l i = if i == 1 then l else (l!!(i-1)):[l!!q| q <- [1..i-2]] ++ [head l] ++ [l!!q|q <- [i..length l - 1]]

    -- Задача 5.a -----------------------------------------
    forwardStep :: Row -> Linear -> Linear
    forwardStep r l = [sumRow i (calcSimple r (head i))| i <- l]

    sumRow :: Row -> Row -> Row
    sumRow r1 r2 = [r1!!i + r2!!i |i <- [1..length r1-1]]

    calcSimple :: Row -> Rational -> Row
    calcSimple r rat = multiply (-rat/head r) r

    multiply :: Rational -> Row -> Row
    multiply r p = [r*i|i<-p]

    -- Задача 5.b -----------------------------------------
    reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
    reverseStep r ps = coef ((-1)/head r) ( add [(0, - last r)](foldr add [] [coef (r!!i) (ps!!(i-1))|i <- [1..length r -2]])):ps

    -- Задача 6 -----------------------------------------
    gauss :: Int -> Linear -> Maybe [PolinomOne]
    gauss i l = gaussHelp l i

    recombine :: Int -> Linear -> Linear
    recombine i l = [[q!!el|el <- [i-1..length q - 1]]| q <- l]

    gaussHelp :: Linear -> Int -> Maybe [PolinomOne]
    gaussHelp l iter
            | isSimple l = solveSimple l
            | null l = Just []
            | otherwise = case findRow l of
                Just a -> case gaussHelp (forwardStep (head (exchangeRow l a)) (tail (exchangeRow l a))) (iter + 1) of
                    Just p ->  Just (reverseStep (head (exchangeRow l a)) p)
                    _ -> Nothing
                _ -> case gaussHelp (recombine 2 l) (iter + 1) of
                    Just p -> Just ([(iter, 1)]:p)
                    _ -> Nothing


    -- Задача 7.a -----------------------------------------
    testEquation :: [PolinomOne] -> Row -> Bool
    testEquation ps r = length (helpEq ps r) == 1 && fst (head (helpEq ps r)) == 0

    helpEq :: [PolinomOne] -> Row -> [(Int, Rational)]
    helpEq ps r = foldr add [] [coef (r!!i) (ps!!i)|i<-[0..length ps-1]]
    -- Задача 7.b -----------------------------------------
    testLinear :: [PolinomOne] -> Linear -> Bool
    testLinear ps ls = and [testEquation ps l|l <- ls]

    -- Задача 8 -----------------------------------------
    solving :: Linear -> Solution
    solving l = case gauss 1 l of
        Nothing -> Empty
        Just ps -> if and [length p == 1 && fst (head p) == 0|p <- ps] then One [snd (head p)|p <- ps] else Many ps

    -------------------------------------------------------
    pol0, pol1, pol2, pol3, pol4 :: PolinomOne
    pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
    pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
    pol2 = [(0,15), (4,3),(5,1)]
    pol3 = [(0,-10), (2,7), (4,-3)]
    pol4 = [(0,-26/15), (2,3), (3,5/7)]

    test0, test1, test2, test3, test3a, test4 :: Linear
    test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
    test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
    test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
    test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
    test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
    test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

    res3, res4 :: [PolinomOne]
    res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
    res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

    sol1,sol2,sol3,sol4 :: Solution
    sol1 = Empty
    sol2 = Empty
    sol3 = Many res3
    sol4 = One [62/15, -17/15, -4/3]


