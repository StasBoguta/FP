{-# OPTIONS_GHC -Wall #-}
module Fedorov07 where

type GraphS = (Int,[(Int,Int)])
type Graph  = [[Int]]


-- all nodes of graph g
nodes:: Graph -> [Int]
nodes g = [0..(length g - 1)]

-- all edges of graph g
edges:: Graph -> [(Int, Int)]
edges g = [(x,y) | x <- nodes g, y <- g!!x]

-- neighbours of node v in graph g
adj:: Graph -> Int -> [Int]
adj g v = g !! v

-- edge is in graph
edgeIn:: Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)

keepDup:: [Int] -> [Int]
keepDup [] = []
keepDup (x:xs)
      | elem x xs = x : keepDup xs
      | otherwise = keepDup xs

delDup:: [Int] -> [Int]
delDup [] = []
delDup (x:xs)
      | elem x xs = delDup xs
      | otherwise = x : delDup xs

delExtraDup:: [[Int]] -> [[Int]]
delExtraDup [] = []
delExtraDup (x:xs)
    | or [eqxint x y | y <- xs] = delExtraDup xs
    | otherwise = x : delExtraDup xs


goNodes:: Graph -> Int -> [Int]
goNodes gr v = snd $ until cond (oneStep gr) ([v],[])

cond:: ([Int], [Int]) -> Bool
cond (ns, _) = ns == []

oneStep:: Graph -> ([Int], [Int]) -> ([Int], [Int])
oneStep gr (ns, os) = 
    let old = ns ++ os
        ns1 = [n | v <- ns, n <- gr!!v]
        ns2 = filter (`notElem` old) ns1
        new = delDup ns2
    in (new, old)

eqxint:: [Int] -> [Int] -> Bool
eqxint xs1 xs2 = and [elem x xs2 | x <- xs1]


allWays:: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW:: ([[[Int]]]) -> Bool
condW = null . head

stepW:: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) = [t:w | w@(x:xs) <- wsn, notElem x xs, t <- gr!!x] : wss
stepW _ [] = error ""

nonRec:: [Int] -> Bool
nonRec xs = length(keepDup xs) == 0

rev:: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]

chosen:: [[Int]] -> ([Int] -> Int) -> [[Int]]
chosen xxs func = minPos
    where 
        minLen = func [length ys | ys <- xxs]
        minPos = [xs | xs <- xxs, length(xs) == minLen]


-----------------------------------------------------







-- ������ 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary gr = noDupEdg && inLimits && noDupNodOrien
    where 
            lel = last $ nodes gr
            inLimits       = and [y <= lel && y >= 0 | x <- gr, y <- x]
            noDupNodOrien  = and [x1 /= x2 && (x2,x1) `elem` (edges gr)| (x1, x2) <- edges gr]
            noDupEdg       = and [length (keepDup x) == 0 | x <- gr]

-- ������ 2 ------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph gr = (length (nodes gr) - 1, xs)
    where xs = [(x, y) | x <- [0..length gr - 1], y <- gr !! x]

-- ������ 3 ------------------------------------
toGraph :: GraphS -> Graph 
toGraph (nu, xs) = [func b | b <- [0..nu]]
    where func inp = [y | (x,y) <- xs, x == inp] 

-- ������ 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay gr start finish = if null [ ys | ys <- res, head ys == finish, last ys == start] then [] else rev $ head $ chosen [ ys | ys <- res, head ys == finish, last ys == start] minimum
    where  
        filterized = [filter (nonRec) xs | xs <- allWays gr start]
        res = [ys | xs <- filterized, ys <- xs]

-- ������ 5 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting gr = length (goNodes gr 0) == length(nodes gr)

-- ������ 6 ------------------------------------
components :: Graph -> [[Int]] 
components gr = if isConnecting gr then [goNodes gr 0] else final
    where 
        func xs n
            | maximum xs /= length(nodes gr) - 1 = [xs] ++ func (goNodes gr (n+1)) (n+1)
            | otherwise = [xs]
        final = delExtraDup $ func (goNodes gr 0) 0

-- ������ 7 ------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity gr inp = if isConnecting gr then length res - 1 else error "Graph is unconnected"
    where res = head $ chosen [shortWay gr inp n | n <- [0..length (nodes gr) - 1]] maximum

-- ������ 8 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter gr = maximum [eccentricity gr n | n <- [0..length (nodes gr) - 1]]

findRadius :: Graph -> Int 
findRadius gr   = minimum [eccentricity gr n | n <- [0..length (nodes gr) - 1]] 

-- ������ 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = [n | n <- [0..length (nodes gr) - 1], eccentricity gr n == findRadius gr] 

-- ������ 10 ------------------------------------
shortWayAll :: Graph -> Int -> Int -> [[Int]] 
shortWayAll gr start finish = chosen [rev ys | ys <- res, head ys == finish, last ys == start] minimum
    where  
        filterized = [filter (nonRec) xs | xs <- allWays gr start]
        res = [ys | xs <- filterized, ys <- xs]

--------------------------
gr1S, gr2S, gr3S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])
gr3S = (4, [(0,3), (1,3), (2,3), (3,0), (3,1), (3,2), (3,4), (4,3)])

gr1, gr2, gr3, gr4, gr5:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
gr3 = [[1,2,3], [0,3], [0,3], [0,1,2], []]
gr4 = [[0],[1],[2],[3]]
gr5 = [[1,4,3], [0,2,4], [1,4,3], [0,2,4], [0,1,2,3]]
