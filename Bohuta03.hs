{-# OPTIONS_GHC -Wall #-}
module Bohuta03 where

    type Graph  = [[Int]]

    adj :: Graph -> Int -> [Int]
    adj g v = g !! v

    nodes :: Graph -> [Int]
    nodes g = [0..(length g - 1)]

    edgeIn :: Graph -> (Int, Int) -> Bool
    edgeIn g (x,y) = y `elem` g!!x

    edges :: Graph -> [(Int,Int)]
    edges g = [(x,y) | x<-nodes g, y <- g!!x]

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

    insert:: Int -> [Int] -> [Int]
    insert v xs = (++) (filter (v>) xs) (v : filter (v<=) xs)

    sortInsert :: [Int] -> [Int]
    sortInsert = foldr insert []

    myMax::[Int] -> Int
    myMax xs
      | length xs == 1 = head xs
      | null xs = 0
      | otherwise = max (head xs) (myMax (tail xs))

    allWays :: Graph -> Int -> [[[Int]]]
    allWays gr v = until condW (stepW gr) [[[v]]]

    condW :: [[[Int]]] -> Bool
    condW wss = null ( head wss)

    stepW :: Graph -> [[[Int]]] -> [[[Int]]]
    stepW gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, x `notElem` xs, t<- gr!!x] : wss
    stepW _ []  = error "allWays:stepW"

    isGraph1::[Int] -> Int -> Bool
    isGraph1 xs l = myMax xs < l && sortInsert xs == sortInsert (set xs)

    isGraph :: Graph -> Bool
    isGraph xs = and [isGraph1 i (length xs)| i <- xs]

    isTournament :: Graph -> Bool
    isTournament xs = isGraph xs && and [not (edgeIn xs (i,i)) &&  (i==q || edgeIn xs (i,q) && not (edgeIn xs (q,i)) || edgeIn xs (q,i) && not (edgeIn xs (i,q)))| i <- [0..length xs-1], q <- [0..length xs-1]]

    isTransitive :: Graph -> Bool
    isTransitive xs = isGraph xs && and [not (edgeIn xs (snd r, i)) || edgeIn xs (fst r, i)|r <- edges xs, i <- [0..length xs -1]]

    buildTransitiveAdd2:: Graph -> Int -> Int -> [Int]
    buildTransitiveAdd2 g i c
      | c == length g = []
      | edgeIn g (i,c) = c:buildTransitiveAdd2 g i (c+1)
      | otherwise = buildTransitiveAdd2 g i (c+1)

    help::Graph->Int->[Int]->[Int]
    help g p ar = if p `elem` ar then ar else foldr (help g) (p:ar) ((!!) g p)

    buildTransitive :: Graph -> Graph
    buildTransitive xs = [sortInsert (set (foldr (help xs) [] i))| i <- xs]

    filterWay::Int -> [Int] -> Bool
    filterWay a ar = a == head ar

    allWaysFiltered::Graph -> Int -> Int -> [[[Int]]]
    allWaysFiltered g a b = [filter (filterWay b) i|i <- allWays g a]

    getLarge:: [[[Int]]] -> Int -> [Int]
    getLarge arr i
      | i == length arr = []
      | not (null ((!!) arr i)) = (!!) ((!!) arr i) 0
      | otherwise = getLarge arr (i+1)

    myReverse::[Int] -> [Int]
    myReverse g = if null g then [] else last g : myReverse (init g)

    longWay :: Graph -> Int -> Int -> Maybe [Int]
    longWay g a b = if null (getLarge (allWaysFiltered g a b) 0) then Nothing else Just(myReverse (getLarge (allWaysFiltered g a b) 0))

    gamiltonWay :: Graph -> Maybe [Int]
    gamiltonWay g = if null (getLarge (allWaysFiltered g 0 0) 0) || length(getLarge (allWaysFiltered g 0 0) 0) /= length g + 1 then Nothing else Just(myReverse (getLarge (allWaysFiltered g 0 0) 0))

    isAcyclic :: Graph -> Bool
    isAcyclic g = and [ length(getLarge (allWaysFiltered g i i) 0) == 1 |i <- [0..length g -1]]

    mySwap::[Int] -> Int -> Int -> [Int]
    mySwap xs a b
      | null xs = []
      | head xs == a = head (tail xs) : (head xs : tail (tail xs))
      | otherwise = head xs : mySwap (tail xs) a b

    toSwap::[Int] -> Int -> Int -> [Int]
    toSwap xs i q = mySwap xs ((!!) xs i) ((!!) xs q)

    makeSort::Graph -> [Int] -> Int ->Int -> [Int]
    makeSort g gs i q
      | i == length g = gs
      | edgeIn g (i,q) =
      if q == (length g -1)
      then makeSort g (toSwap gs i q) (i+1) 0
      else makeSort g (toSwap gs i q) (i+1) (q+1)
      | q == (length g -1) = makeSort g gs (i+1) 0
      | otherwise = makeSort g gs (i+1) (q+1)


    topolSort :: Graph -> Maybe [Int]
    topolSort g = if not (isAcyclic g) then Nothing else Just (makeSort g [0 .. length g - 1] 0 0)

    isTopolSort :: Graph -> [Int] -> Bool
    isTopolSort g ts = ts == (if not (isAcyclic g) then [] else makeSort g [0 .. length g - 1] 0 0)

    gr1, gr2, gr3, gr4:: Graph
    gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
    gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
    gr3 = [[1],[2],[3],[1],[0,3]]
    gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]