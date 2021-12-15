{-# OPTIONS_GHC -Wall #-}
module Bohuta00 where

        type Graph  = [[Int]]
        data BinTreeM a = EmptyM
                        | NodeM a Int (BinTreeM a) (BinTreeM a)
                        deriving (Show, Eq)
        type System = [(String,Recur)]
        data Recur = Zero | Succ | Sel Int Int
                | Super Recur [Recur]
                | Prim Recur Recur
                | Mini Recur Int
                | Name String  deriving (Show, Eq)

        -- Задача 1 -----------------------------------------
        group :: Eq a => [a] -> [[a]]
        group = groupHelp [] []

        groupHelp :: Eq a => [[a]] -> [a] -> [a] -> [[a]]
        groupHelp res el cur
                | null cur = if null el then res else res ++ [el]
                | null el = groupHelp res (el ++ [head cur]) (tail cur)
                | last el == head cur = groupHelp res (el ++ [head cur]) (tail cur)
                | otherwise = groupHelp (res ++ [el]) [] cur


        -- Задача 2 -----------------------------------------
        bagSubbag :: String -> String -> Bool
        bagSubbag s1 s2 = and [calcIn i s1 <= calcIn i s2 |i<-s1]

        calcIn :: Char -> String -> Int
        calcIn c s = length (filter (==c) s)
        -- Задача 3 -----------------------------------------
        bagUnion :: String -> String -> String
        bagUnion = bagUnionHelp []

        addN :: Char -> Int -> String
        addN c i
                | i == 1 = [c]
                | otherwise = c : addN c (i-1)

        bagUnionHelp :: String -> String -> String -> String
        bagUnionHelp res s1 s2
                | null s1 = res ++ s2
                | null s2 = res ++ s1
                | calcIn (head s1) s1 >= calcIn (head s1) s2 = bagUnionHelp (res ++ addN (head s1) (calcIn (head s1) s1)) (filter (/= head s1) s1) (filter (/= head s1) s2)
                | otherwise = bagUnionHelp (res ++ addN (head s1) (calcIn (head s1) s2)) (filter (/= head s1) s1) (filter (/= head s1) s2)

        -- Задача 4 -----------------------------------------
        frequency :: [Int] -> [(Int,Int)]
        frequency = frequencyHelp []

        frequencyHelp :: [(Int,Int)] -> [Int] -> [(Int,Int)]
        frequencyHelp res r
                | null r = res
                | otherwise = frequencyHelp (res ++ [(head r,length (filter (== head r) r))]) (filter (/= head r) r)
        -- Задача 5 -----------------------------------------
        components :: Graph -> [[Int]]
        components gr = componentsH [] gr 0

        componentsH:: [[Int]] -> Graph -> Int -> [[Int]]
        componentsH arr g vn
                | vn == length g = arr
                | isInArr arr vn = componentsH arr g (vn+1)
                | otherwise = componentsH (arr ++ [addIfNotCon vn (componentsHelp [] (g!!vn) g)]) g (vn+1)

        addIfNotCon::Int -> [Int] -> [Int]
        addIfNotCon i ir = i:filter (/=i) ir

        componentsHelp :: [Int] -> [Int] -> Graph -> [Int]
        componentsHelp vs v gr
                | null v = vs
                | isIn vs (head v) = componentsHelp vs (tail v) gr
                | otherwise = componentsHelp (componentsHelp (vs ++ [head v]) (gr!!head v) gr) (tail v) gr

        isInArr :: [[Int]] -> Int -> Bool
        isInArr arr el = or [isIn i el|i <- arr]

        isIn:: [Int] -> Int -> Bool
        isIn arr el = not (not (any (==el) arr))
        -- Задача  6 -----------------------------------------
        eccentricity :: Graph -> Int -> Int
        eccentricity gr v = maximum [helpEccent gr v i |i <- [0..length gr -1]]

        helpEccent :: Graph -> Int -> Int -> Int
        helpEccent gr v x = length (shortWay gr v x) - 1

        shortWay :: Graph -> Int -> Int -> [Int]
        shortWay gr a b
                | null (helpWay b (head (until (helpCWay b) (helpSWay gr) [[[a]]]))) = []
                | otherwise = reverse(head(helpWay b (head (until (helpCWay b) (helpSWay gr) [[[a]]])))) where
                         helpWay bs wss = [ws | ws <- wss, bs `elem` ws]
                         helpCWay i xs = null(head xs) || not(null(helpWay i (head xs)))
                         helpSWay _ [] = undefined
                         helpSWay grr (w:ws)  = [t:(x:xs) | (x:xs) <- w, x `notElem` xs, t<- grr!!x] : (w:ws)


        -- Задача 7 -----------------------------------------
        findDiameter :: Graph -> Int
        findDiameter gr
                |null [eccentricity gr currNode | currNode <- [0..length gr -1]] = 0
                |otherwise = maximum [eccentricity gr currNode | currNode <- [0..length gr -1]]

        findRadius :: Graph -> Int
        findRadius gr
                |null [eccentricity gr currNode | currNode <- [0..length gr -1]] = 0
                |otherwise = minimum [eccentricity gr currNode | currNode <- [0..length gr -1]]
        -- Задача 8 -----------------------------------------
        findCenter :: Graph -> [Int]
        findCenter g
                | and [not (null (shortWay g 0 x)) | x<-[1..(length g - 1)]] = [v | v <- [0.. length g -1], eccentricity g v == findRadius g]
                | otherwise  = []


        --- Задача 9 ----------------------------------------
        elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
        elemSearch EmptyM _ = False
        elemSearch (NodeM tv _ tl tr) v
                | v == tv = True
                | v < tv = elemSearch tl v
                | otherwise  = elemSearch tr v

        -- Задача 10 ------------------------------------
        insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
        insSearch EmptyM v = NodeM v 1 EmptyM EmptyM
        insSearch (NodeM tv tn tl tr) v
                | v == tv = NodeM tv (tn+1) tl tr
                | v < tv = NodeM tv tn (insSearch tl v) tr
                | otherwise = NodeM tv tn tl (insSearch tr v)

        -- Задача 11 ------------------------------------
        delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
        delSearch EmptyM _=  EmptyM
        delSearch (NodeM tv tn tl tr) v
                | v == tv =
                        if tn>1
                                then NodeM tv (tn-1) tl tr
                        else delElem (NodeM tv tn tl tr)
                | v < tv = NodeM tv tn (delSearch tl v) tr
                | otherwise = NodeM tv tn tl (delSearch tr v)

        delElem :: (Ord a) => BinTreeM a -> BinTreeM a
        delElem (NodeM _ _ tl  EmptyM) = tl
        delElem (NodeM _ _ EmptyM  tr) = tr
        delElem (NodeM _ tn tl tr) = NodeM (lastElem tr) tn tl tr
        delElem _ = undefined


        lastElem :: (Ord a) => BinTreeM a -> a
        lastElem (NodeM tv _ EmptyM  _) = tv
        lastElem (NodeM _ _ tl _) = lastElem tl
        lastElem _ = undefined

        -- Задача 12 ------------------------------------
        sortList :: (Ord a) => [a] -> [a]
        sortList l = toList (foldl insSearch EmptyM l)

        toList :: BinTreeM a -> [a]
        toList EmptyM = []
        toList (NodeM tv tn tl tr) = toList tl ++ addNT tv tn ++toList tr

        addNT::a-> Int->[a]
        addNT v n
                | n == 1 = [v]
                | otherwise = v:addNT v (n-1)

        -- Задача 13 ------------------------------------
        isNumbConst :: System -> Recur -> Bool
        isNumbConst s (Name f) = isNumbConst s (snd (head (filter (\(x,_) -> x==f) s)))
        isNumbConst _ (Sel _ _) = False
        isNumbConst _ Succ = False
        isNumbConst _ Zero = True
        isNumbConst _ (Super _ _) = False
        isNumbConst _ (Prim _ _) = False
        isNumbConst _ (Mini _ _) = False


        -- Задача 14 ------------------------------------
        evRank :: System -> Recur -> Int
        evRank _ Succ = 1
        evRank _ Zero = 1
        evRank _ (Sel n _ ) = n
        evRank s (Super _ ls) = evRank s (head ls)
        evRank s (Prim _ n) = evRank s n -1
        evRank s (Mini r _) = evRank s r -1
        evRank s (Name ss) = evRank s (funct s ss) where
                funct [] _ = undefined
                funct (x:xs) f
                        | fst x == f = snd x
                        | otherwise = funct xs f

        -- Задача 15 ------------------------------------
        isNames :: System -> Bool
        isNames syst= check syst && checkRec (reverse syst) where
                checkRec [] = True
                checkRec (x:sys) = isRecur sys (snd x) && checkRec sys
                check [] = True
                check (x:xs) = not (checkId xs (fst x)) && check xs where
                        checkId [] _ = False
                        checkId (x1:x1s) s
                                | fst x1 == s = True
                                | otherwise = checkId x1s s

        -- Задача 16 ------------------------------------
        isRecur :: System -> Recur -> Bool
        isRecur _ Zero = True
        isRecur _ Succ = True
        isRecur _ (Sel _ _)  = True
        isRecur s (Prim r1 r2)  = isRecur s r1 && isRecur s r2
        isRecur s (Mini r1 _)  = isRecur s r1
        isRecur s (Super r1 r2) = isRecur s r1 && and [isRecur s x | x <- r2]
        isRecur s (Name ss) = checkRecurName (reverse s) ss where
                checkRecurName [] _ = False
                checkRecurName (x:xs) s1 = fst x == s1 || checkRecurName xs s1

        -- Задача 17 ------------------------------------
        eval :: System -> Recur -> [Int] -> Int
        eval _ Zero _= 0
        eval _ Succ xs = head xs +1
        eval _ (Sel n k) xs
                | k<=n && length xs >= n = xs!!(k-1)
                | otherwise = 0
        eval s (Super f gn) xs = eval s f [eval s g xs|g<-gn]
        eval s (Name nm) xs = if containsRecur s nm then eval s (getRecur s nm) xs else undefined 
                where
                        containsRecur s1 ss
                                | null s1 = False
                                | fst(head s1)==ss = True
                                | otherwise = containsRecur (tail s1) ss
                        getRecur s1 ss
                                | null s1 = undefined
                                | fst(head s1)==ss = snd (head s1)
                                | otherwise = getRecur (tail s1) ss
        eval s (Prim r1 r2) xs =  last(fst(until cond (stepPrim s r2) (init xs++[0]++[eval s r1 (init xs)],last xs))) where
                stepPrim ss r (xss,n) = (init(init xss)++[last (init xss)+1]++[eval ss r xss], n)
                cond (xss,n) = last(init xss)==n
        eval _ (Mini _ _) _ = 0




        -- Задача 18 ------------------------------------
        evalPart :: System -> Recur -> [Int] -> Maybe Int
        evalPart _ Zero _= Just 0
        evalPart _ Succ xs = Just (head xs +1)
        evalPart _ (Sel n k) xs
                | k<=n && length xs >= n = Just (xs!!(k-1))
                | otherwise = Just 0
        evalPart s (Super f gn) xs = evalPart s f [eval s g xs|g<-gn]
        evalPart s (Name nm) xs= if containsRecur s nm then evalPart s (getRecur s nm) xs else Nothing 
                        where
                                containsRecur s1 ss
                                        | null s1 = False
                                        | fst(head s1)==ss = True
                                        | otherwise = containsRecur (tail s1) ss
                                getRecur s1 ss
                                        | null s1 = undefined
                                        | fst(head s1)==ss = snd (head s1)
                                        | otherwise = getRecur (tail s1) ss
        evalPart s (Prim r1 r2) xs = case evalPart s r1 (init xs) of
                        Just res -> case fst (until condM (sstep s r2) (Just (init xs++[0]++[res]),last xs)) of
                                Just ys -> Just (last ys)
                                _ -> Nothing
                        _ -> Nothing  
        evalPart s (Mini g i) vl  = if null xs then Nothing else Just (head xs)
                                where xs = filter (\x -> eval s g (vl ++ [x]) == 0) [0..i]

        sstep :: System->Recur->(Maybe [Int],Int)->(Maybe [Int],Int)
        sstep s r1 (Just xs,n) = case evalPart s r1 xs of
                                Just r ->(Just(init(init xs)++[last (init xs)+1]++[r]), n)
                                _ ->(Nothing,n)
        sstep _ _ (_,n) = (Nothing, n)

        condM :: (Maybe[Int],Int)->Bool
        condM (Just xs,n) = last(init xs)==n
        condM (_ , _) = True

        ---------------------Тестові дані - Графи -------
        gr1, gr2:: Graph
        gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
        gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]

        ---------------------Тестові дані - Дерева пошуку -------
        bm :: BinTreeM Char
        bm = NodeM  't' 2
                (NodeM 'a' 1  EmptyM
                        (NodeM 'e' 1
                                (NodeM 'd' 2 EmptyM EmptyM)
                                (NodeM 'f' 1 EmptyM EmptyM)
                        )
                )
                (NodeM 'w' 2  EmptyM EmptyM)

        ---------------------Тестові дані - Рекурсивні функції -------
        syst1, syst2 :: System
        syst1 = [("const0", Zero)
                , ("const0v2", Super Zero [Sel 2 1])
                , ("const0v3", Super Zero [Sel 3 1])
                , ("const1v2", Super Succ [Super Zero [Sel 2 1]])
                , ("const2", Super Succ [Super Succ [Zero]])
                , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ]))
                , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))
                , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))
                , ("subtract1", Prim Zero (Sel 2 1))
                , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))
                , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])
                , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])
                , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])
                , ("subtractionPart", Mini (Name "subtractionAbs3") 100)
                ]
        syst2 = [("f1", Super Succ [Zero])
                ,("f2", Super Succ [Name "f2"])
                ]
