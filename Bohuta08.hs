{-# OPTIONS_GHC -Wall #-}
module Bohuta08 where

data BinTree a = EmptyB
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a
               | Node2 (Tree23 a) a (Tree23 a)
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- ������� 2-3-������!!!
                   deriving (Eq, Show)

-- ������ 1 -----------------------------------------	

isLess :: (Ord a) => BinTree a -> a -> Bool
isLess EmptyB _ = True
isLess (Node v l r) pV = v <= pV && isLess l v && isBetween r v pV

isBetween :: (Ord a) => BinTree a -> a -> a -> Bool
isBetween EmptyB _ _ = True
isBetween (Node v l r) mi ma = v > mi && v < ma && isBetween l mi v && isBetween r v ma

isBig :: (Ord a) => BinTree a -> a -> Bool
isBig EmptyB _ = True
isBig (Node v l r) pV = v > pV && isLess l v && isBetween r pV v && isBig r v

isSearch :: (Ord a) => BinTree a -> Bool
isSearch EmptyB = True
isSearch (Node v l r) = isLess l v && isBig r v

-- ������ 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch EmptyB _ = False
elemSearch (Node v l r) el
                | el > v = elemSearch r el
                | el < v = elemSearch l el
                | otherwise = True


-- ������ 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a
insSearch EmptyB el = Node el EmptyB EmptyB
insSearch (Node v l r) el
                | el > v = Node v l (insSearch r el)
                | otherwise = Node v (insSearch l el) r

-- ������ 4 -----------------------------------------
del :: (Ord a) => BinTree a -> BinTree a
del EmptyB = EmptyB
del (Node _ l EmptyB) = l
del (Node _ _ r) = r


delSearch :: (Ord a) => BinTree a -> a -> BinTree a
delSearch EmptyB _ = EmptyB
delSearch (Node v l r) el
                | el > v = Node v l (delSearch r el)
                | el < v = Node v (delSearch l el) r
                | otherwise = del (Node v l r)

-- ������ 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList ar = readT (insertT EmptyB ar)

insertT :: (Ord a) => BinTree a -> [a] -> BinTree a
insertT = foldl insSearch

readT :: (Ord a) => BinTree a -> [a]
readT EmptyB = []
readT (Node v l r) = readT l ++ (v: readT r)


-- ������ 6-----------------------------------------
isTree23  :: (Ord a) => Tree23 a -> Bool
isTree23 (Leaf _)  = True
isTree23 Empty23 = True
isTree23 (Node2 (Leaf l) v (Leaf r)) = v <= r && v >= l
isTree23 (Node3 (Leaf l) lv (Leaf m) rv (Leaf r)) = lv <= m && lv >= l && rv <= r && rv >= m
isTree23 (Node2 l v r) = isMax l v && isMin r v && isOneLevel l == isOneLevel r
isTree23 (Node3 l lv m rv r) = isMax l lv && isMin m lv && isMax m rv && isMin r rv && lv <= rv && isOneLevel l == isOneLevel m && isOneLevel m == isOneLevel r

isMax :: (Ord a) => Tree23 a -> a -> Bool
isMax (Leaf a) v = v >= a
isMax Empty23 _ = True
isMax (Node2 l x r) v = isMax l x && isMin r x && v >= x
isMax (Node3 l lv m rv r) v = isMax l lv && isMin m lv && isMax m rv && isMin r rv && v >= lv && v >= rv && lv <= rv

isMin :: (Ord a) => Tree23 a -> a -> Bool
isMin (Leaf a) v = v <= a
isMin Empty23 _ = True
isMin (Node2 l x r) v = isMax l x && isMin r x && v <= x
isMin (Node3 l lv m rv r) v = isMax l lv && isMin m lv && isMax m rv && isMin r rv && v <= lv && v <= rv && lv <= rv

isOneLevel :: (Ord a) => Tree23 a -> Int
isOneLevel Empty23 = -1
isOneLevel (Leaf _) = 0
isOneLevel (Node2 l _ r) = if isOneLevel l == isOneLevel r then 1 + isOneLevel r else max (isOneLevel l) (isOneLevel r)
isOneLevel (Node3 l _ m _ r) = if isOneLevel l == isOneLevel m && isOneLevel m == isOneLevel r then 1 + isOneLevel l else max (isOneLevel l) (max (isOneLevel m) (isOneLevel r))
-- ������ 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Empty23 _ = False
elemTree23 (Leaf a) v = a==v
elemTree23 (Node2 l x r) v = isTree23 (Node2 l x r) && ((x == v) || (elemTree23 l v || elemTree23 r v))
elemTree23 (Node3 l lv m rv r) v = isTree23 (Node3 l lv m rv r) && ((v == lv || v == rv) || (elemTree23 l v || elemTree23 m v || elemTree23 r v))

-- ������ 8-----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 = isTree23 t1 && isTree23 t2 && (toList23 t1 == toList23 t2)

toList23 :: (Ord a) => Tree23 a -> [a]
toList23 Empty23 = []
toList23 (Leaf v) = [v]
toList23 (Node2 l v r) = toList23 l ++ (v:toList23 r)
toList23 (Node3 l lv m rv r) = toList23 l ++ (lv:toList23 m) ++ (rv:toList23 r)
-- ������ 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 t v = case snd (insert v t) of
                Nothing -> fst (insert v t)
                Just tt -> Node2 (fst (insert v t)) (fst tt) (snd tt)

isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm x (Node2 (Leaf l) v (Leaf r)) = (
        if x <= l then Node3 (Leaf x) l (Leaf l) v (Leaf r)
        else if x > l && x < v then Node3 (Leaf l) v (Leaf v) x (Leaf r)
        else if x >= v && x < r then Node3 (Leaf l) x (Leaf v) r (Leaf r)
        else Node3 (Leaf l) r (Leaf r) v (Leaf v), Nothing )

insTerm x (Node3 (Leaf l) lv (Leaf m) rv (Leaf r))
                        | x <= l = (Node2 (Leaf x) l (Leaf l), Just(lv, Node2 (Leaf m) rv (Leaf r)))
                        | x > l && x < lv = (Node2 (Leaf l) x (Leaf x), Just(lv, Node2 (Leaf m) rv (Leaf r)))
                        | x >= lv && x < rv =
                                if x < m then (Node2 (Leaf l) lv (Leaf x), Just(x, Node2 (Leaf m) rv (Leaf r)))
                                else (Node2 (Leaf l) lv (Leaf m), Just(x, Node2 (Leaf x) rv (Leaf r)))
                        | x < r = (Node2 (Leaf l) x (Leaf m), Just(rv, Node2 (Leaf x) r (Leaf r)))
                        | otherwise = (Node2 (Leaf l) lv (Leaf m), Just(rv, Node2 (Leaf r) x (Leaf x)))

insTerm _ _ = undefined

insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode v Empty23 = (Leaf v, Nothing )
insNode v (Leaf l)
                | v < l = (Node2 (Leaf v) l (Leaf l), Nothing )
                |otherwise =  (Node2 (Leaf l) v (Leaf v), Nothing )
insNode v (Node2 l x r)
                | v < x = case snd (insert v l) of
                        Nothing -> (Node2 (fst (insert v l)) x r, Nothing )
                        Just t -> (Node3 (fst (insert v l)) (fst t) (snd t) x r, Nothing )
                | otherwise = case snd (insert v r) of
                        Nothing -> (Node2 l x (fst (insert v r)), Nothing )
                        Just t -> (Node3 l x (fst (insert v r)) (fst t) (snd t),Nothing )
insNode v (Node3 l lv m rv r)
                | v < lv = case snd (insert v l) of
                        Nothing -> (Node3 (fst (insert v l)) lv m rv r,Nothing )
                        Just t -> (Node2 (fst (insert v l)) (fst t) (snd t), Just (lv, Node2 m rv r))
                | v >= lv && v < rv = case snd (insert v m) of
                        Nothing  -> (Node3 l lv (fst (insert v m)) rv r, Nothing )
                        Just t -> (Node2 l lv (fst (insert v m)), Just (fst t, Node2 (snd t) rv r))
                | otherwise  = case snd (insert v r) of
                        Nothing -> (Node3 l lv m rv (fst (insert v r)), Nothing )
                        Just t -> (Node2 l lv m, Just (rv, Node2 (snd t) (fst t) (fst (insert v r)))) 

---  ������� ������ 
bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB)
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB)
                       EmptyB)

---- 2-3-������
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1))
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5))
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8))
            )            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )
