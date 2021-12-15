{-# OPTIONS_GHC -Wall #-}
module Fedorov08 where

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-������ ������� t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- ������� �������������� B-������  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

---------------------------------------

-- finds main elem in searchTree
searchElem:: (Ord a) => BinTreeM a -> Maybe a
searchElem (NodeM v _ _ _) = Just v
searchElem EmptyM = Nothing

noJust:: (Ord a) => Maybe a -> a
noJust (Just el) = el
noJust Nothing = undefined

-- get all elems of search tree
getAllElems:: (Ord a) => BinTreeM a -> [a]
getAllElems tree = func tree
   where
      func (NodeM v _ tl tr)
         | tl /= EmptyM && tr /= EmptyM = v : func tl ++ func tr
         | tl /= EmptyM = v : func tl
         | tr /= EmptyM = v : func tr
         | otherwise = [v]
      func EmptyM = []

-- all elements are smaller/bigger than given element
alsmal:: (Ord a) => [a] -> a -> Bool
alsmal xr r = length (filter(> r) xr) == 0 

albig:: (Ord a) => [a] -> a -> Bool
albig xr r = length (filter(< r) xr) == 0 

-- check for isSearch
midCheck:: (Ord a) => BinTreeM a -> Bool
midCheck (NodeM v _ tl tr) = smaller && bigger
   where
      smaller = alsmal (getAllElems tl) v
      bigger = albig (getAllElems tr) v
midCheck EmptyM = False


-- checks if tl < v < tr
-- operSearch:: (Ord a) => BinTreeM a -> Bool
-- operSearch (NodeM v k tl tr) = if check then searchElem (NodeM v k tl tr) > (searchElem tl) && searchElem (NodeM v k tl tr) < (searchElem tr) else True
--    where check = tl /= EmptyM && tr /= EmptyM
-- operSearch EmptyM = True

-- next step for routing in searching
route:: (Ord a) => BinTreeM a -> a -> BinTreeM a
route (NodeM v _ tl tr) el = if el < v then tl else tr
route EmptyM _ = EmptyM



-- finds specific tree with the element in search tree
getTr:: (Ord a) => BinTreeM a -> a -> BinTreeM a
getTr tre el = func tre
   where 
      func (NodeM v k tr tl)
         | el == v = (NodeM v k tr tl)
         | otherwise = func (route (NodeM v k tr tl) el)
      func EmptyM = EmptyM

-- change existing element in the search tree
chTr:: (Ord a) => BinTreeM a -> BinTreeM a -> (Int -> Int -> Int) -> BinTreeM a
chTr tree1 tree2 f = func tree1
   where 
      (NodeM _ kr _ _) = (getTr tree2 (findM tl1))
      tree3 = if (f k1 1) == 0 then check else (NodeM v1 (f k1 1) tl1 tr1)
      (NodeM v1 k1 tl1 tr1) = tree2
      func (NodeM v k tl tr)
         | tree1 == tree2 = if (f k 1) == 0 then check else (NodeM v (f k 1) tl tr)
         | tl == tree2 = (NodeM v k tree3 tr)
         | tr == tree2 = (NodeM v k tl tree3)
         | otherwise = if route (NodeM v k tl tr) (noJust $ searchElem tree2) == tl then (NodeM v k (func (route (NodeM v k tl tr) (noJust $ searchElem tree2))) tr) else (NodeM v k tl (func (route (NodeM v k tl tr) (noJust $ searchElem tree2))))
      func EmptyM = EmptyM 
      check 
         | tl1 == EmptyM && tr1 == EmptyM = EmptyM
         | tl1 == EmptyM = tr1
         | tr1 == EmptyM = tl1
         | otherwise = (NodeM (findM tl1) kr (delSearch tl1 (findM tl1)) tr1)

-- adds new Element into the tree
newPl:: (Ord a) => BinTreeM a -> a -> BinTreeM a
newPl tree el = func tree
   where
      func (NodeM v k tl tr)
         | tl == EmptyM && tr == EmptyM = if el > v then (NodeM v k tl (NodeM el 1 EmptyM EmptyM)) else (NodeM v k (NodeM el 1 EmptyM EmptyM) tr)
         | tl == EmptyM && el < v = (NodeM v k (NodeM el 1 EmptyM EmptyM) tr)
         | tr == EmptyM && el > v = (NodeM v k tl (NodeM el 1 EmptyM EmptyM))
         | otherwise = if route (NodeM v k tl tr) el == tl then (NodeM v k (func (route (NodeM v k tl tr) el)) tr) else (NodeM v k tl (func (route (NodeM v k tl tr) el)))
      func EmptyM = (NodeM el 1 EmptyM EmptyM)


-- build binarySearchTree from an array
buildTr:: (Ord a) => [a] -> BinTreeM a
buildTr ar =  func (insSearch EmptyM (head ar)) (tail ar)
   where 
      func tree array
         | length(array) == 1 = insSearch tree (head array)
         | otherwise          = func(insSearch tree (head array)) (tail array)

writeN:: (Ord a) => a -> Int -> [a]
writeN el num = [el | _ <- [0..num-1]]

-- in-order output of a binary search tree
inorder:: (Ord a) => BinTreeM a -> [a]
inorder (NodeM v k tl tr) 
   | tl /= EmptyM = inorder tl ++ inorder (NodeM v k EmptyM tr)
   | tr /= EmptyM = (writeN v k) ++ inorder tr
   | otherwise = writeN v k
inorder EmptyM = []

-- find maximum el in binTreeM
findM:: (Ord a) => BinTreeM a -> a
findM = maximum . getAllElems  

---------------------------------Btrees --------------------

-- height of a Btree
heightBtr:: (Bounded a, Ord a) => Btree a -> Int
heightBtr (NodeB _ tr) = if tr /= [] then func (head tr) else 0
   where
      func (NodeB _ tr1)
         | tr1 /= [] = 1 + func (head tr1)
         | otherwise = 1

-- smallest element in Btree
smallest:: (Bounded a, Ord a) => Btree a -> a
smallest (NodeB xr tr) = if tr /= [] then func (head tr) else head xr
   where
      func (NodeB xr1 tr1)
         | tr1 /= [] = func (head tr1)
         | otherwise = head xr1

-- biggest element in Btree
biggest:: (Bounded a, Ord a) => Btree a -> a
biggest (NodeB xr tr) = if tr /= [] then func (last tr) else last xr
   where
      func (NodeB xr1 tr1)
         | tr1 /= [] = func (last tr1)
         | otherwise = last xr1
            
-- largest key amount
bigKeyAm:: (Bounded a, Ord a) => Btree a -> Int
bigKeyAm tree = maximum $ func tree
   where 
   func (NodeB xr tr)
      | tr /= [] = length xr : [maximum $ func x | x <- tr]
      | otherwise = [length xr]

-- smallest key amount
smalKeyAm:: (Bounded a, Ord a) => Btree a -> Int
smalKeyAm tree = minimum $ tail $ func tree
   where 
   func (NodeB xr tr)
      | tr /= [] = length xr : [minimum $ func x | x <- tr]
      | otherwise = [length xr]


-- checks if string is sorted
isSorted:: (Ord a) => [a] -> Bool
isSorted ss = and $ func ss
   where 
   func (x:xs)
      | xs /= [] = (x <= head xs) : func xs
      | otherwise = [] 
   func [] = []

-- all nodes in btree are sorted
allSorted:: (Bounded a, Ord a) => Btree a -> Bool
allSorted tree = and $ func tree
   where 
   func (NodeB xr tr)
      | tr /= [] = isSorted xr : [and $ func x | x <- tr]
      | otherwise = [isSorted xr]

-- keys of nodes of a tree
neededKeys:: (Bounded a, Ord a) => Btree a -> [[a]]
neededKeys (NodeB _ tr) = [head xr : last xr : [] | (NodeB xr _) <- tr]

-- tree has okayish nodes
isRight:: (Bounded a, Ord a) => Btree a -> Bool
isRight (NodeB xr tr) = if tr /= [] then and $ func 0 else True
   where
      keys = neededKeys (NodeB xr tr)
      func num
         | num == length xr - 1 && length xr /= 1 = [last xr <= head (last keys)]
         | length xr == 1 = [head xr >= last (head keys) && head xr <= head (last keys)]
         | otherwise = (head(drop num xr) >= last(head(drop num keys)) && head(drop num xr) <= head(head(drop (num+1) keys))) : func (num+1)
         

-- all subtrees in tree has okayish nodes
isAllRight:: (Bounded a, Ord a) => Btree a -> Bool
isAllRight tree = and $ func tree
   where
      func (NodeB xr tr)
         | tr /= [] = isRight (NodeB xr tr) : [and $ func x | x <- tr]
         | otherwise = [isRight (NodeB xr tr)]


sort:: (Bounded a, Ord a) => [a] -> [a]
sort xs = func xs
   where 
      func ss
         | length ss - amount /= 0 = writeN (minimum ss) amount ++ func [x | x <- ss, x /= minimum ss]
         | otherwise = writeN (minimum ss) amount
         where amount = length([x | x <- ss, x == minimum ss])


getAllKeys:: (Ord a) => Btree a -> [a]
getAllKeys (NodeB xr tr)
         | tr /= [] = xr ++ concat [getAllKeys x | x <- tr]
         | otherwise = xr


findPosSingle:: (Ord a) => [a] -> a -> Int
findPosSingle xr el = func xr 0
   where
      func yr num
         | yr == [] = length xr
         | el > head yr = func (tail yr) (num + 1)
         | otherwise = num

insertElem:: (Ord a) => [a] -> a -> [a]
insertElem xr el = take (findPosSingle xr el) xr ++ el : drop (findPosSingle xr el) xr


-- finds exact node where input should be
findPos:: (Ord a) => Int -> Btree a -> a -> Btree a
findPos ord (NodeB xr1 tr1) el = if tr1 == [] then (NodeB (insertElem xr1 el) tr1) else  func (NodeB xr1 tr1) (0::Int)
   where
      func (NodeB xr tr) num
         | tr /= [] && length xr == (ord * 2 - 1) = func (head(drop (findPosSingle xrreb el) trreb)) (num+1)
         | tr /= [] && length xr /= (ord * 2 - 1) = func (head(drop (findPosSingle xr el) tr)) (num + 1)
         | length xr == (ord * 2 - 1) = if num < 2 then test2 ord (NodeB xr1 tr1) (NodeB xr tr) el else fullInsert (NodeB xr1 tr1) (findFath (NodeB xr1 tr1) (NodeB xr tr)) (test2 ord (NodeB xr1 tr1) (NodeB xr tr) el)
         | otherwise =  if num < 2 then test2 ord (NodeB xr1 tr1) (NodeB xr tr) el else fullInsert (NodeB xr1 tr1) (findFath (NodeB xr1 tr1) (NodeB xr tr)) (test2 ord (NodeB xr1 tr1) (NodeB xr tr) el)
         where
            (NodeB xrreb trreb) = test3 ord (NodeB xr1 tr1) (NodeB xr tr) 



-- inserts and rebuilds a Node
test2 :: (Ord a) => Int -> Btree a -> Btree a -> a -> Btree a
test2 num tree1 (NodeB xr2 tr2) el = func tree1
   where 
      func (NodeB xr tr)
         | elem (NodeB xr2 tr2) [x | x <- tr] && length xr2 == (num * 2 - 1) = (NodeB (insertElem xr middle) ((take findNodePos tr) ++ (firstNode : secondNode : (drop (findNodePos +1) tr))))
         | elem (NodeB xr2 tr2) [x | x <- tr] && length xr2 /= (num * 2 - 1) = (NodeB xr ((take findNodePos tr) ++ ((NodeB (insertElem xrch el) trch): (drop (findNodePos +1) tr))))
         | otherwise = if drop (findPosSingle xr (head xr2)) tr /= [] then func (head(drop (findPosSingle xr (head xr2)) tr)) else (NodeB xr2 tr2) 
         where
            middle = head(drop (num - 1) xr2)
            firstPart = if el < middle then insertElem (take (num-1) xr2) el else take (num-1) xr2
            secondPart = if el > middle then insertElem (drop num xr2) el else drop num xr2
            firstNode = (NodeB firstPart [])
            secondNode = (NodeB secondPart [])
            findNodePos = head [x | x <- [0..length tr-1], tr !! x == (NodeB xr2 tr2)]
            (NodeB xrch trch) = head (drop findNodePos tr) 

-- rebuilds node without insertion?
test3 :: (Ord a) => Int -> Btree a -> Btree a -> Btree a
test3 num tree1 (NodeB xr2 tr2) = func tree1
   where 
      func (NodeB xr tr)
         | elem (NodeB xr2 tr2) [x | x <- tr] = (NodeB (insertElem xr middle) ((take findNodePos tr) ++ (firstNode : secondNode : (drop (findNodePos +1) tr))))
         | otherwise = if drop (findPosSingle xr (head xr2)) tr /= [] then func (head(drop (findPosSingle xr (head xr2)) tr)) else (NodeB [middle] (firstNode : secondNode : [] ))
         where
            middle = head(drop (num - 1) xr2)
            firstPart =  take (num-1) xr2
            secondPart = drop num xr2
            firstNode = (NodeB firstPart (take num tr2))
            secondNode = (NodeB secondPart (drop num tr2))
            findNodePos = head [x | x <- [0..length tr-1], tr !! x == (NodeB xr2 tr2)]   



-- getAllNodes:: (Ord a) => Btree a -> [a]
-- getAllNodes tree = func tree
--    where
--       func (NodeB xr tr)
--          | tr /= [] = xr ++ concat [func x | x <- tr]
--          | otherwise = xr

insertNode::(Ord a) => Btree a -> Btree a -> Btree a -> Btree a
insertNode tree1 oldTree (NodeB xr tr) = func tree1
   where
      func (NodeB xr1 tr1)
         | elem oldTree [x | x <- tr1] = (NodeB xr1 ((take findNodePos tr1) ++ ((NodeB xr tr) : drop (findNodePos+1) tr1 )))
         | otherwise = head [func x | x <- tr1]
            where findNodePos = head [x | x <- [0..length tr1-1], tr1 !! x == oldTree]  

fullInsert:: (Ord a) => Btree a -> Btree a -> Btree a -> Btree a
fullInsert tree oldTree newTree = func
   where 
      func 
         | findFath tree oldTree == tree = insertNode tree oldTree newTree
         | otherwise = fullInsert tree (findFath tree oldTree) (insertNode tree oldTree newTree)

findFath:: (Ord a) => Btree a -> Btree a -> Btree a
findFath tree1 (NodeB xr2 tr2) = func tree1
   where
      func (NodeB xr tr)
         | elem (NodeB xr2 tr2) [x | x <- tr] = (NodeB xr tr)
         | otherwise = func $ head (drop (findPosSingle xr (head xr2)) tr) 


-- ������ 1 ------------------------------------
isSearch:: (Ord a) => BinTreeM a -> Bool
isSearch tree = if check tree && midCheck tree then func tree else False
   where 
      func (NodeM v k tl tr)
         | k == 0 = False
         | tl /= EmptyM && tr /= EmptyM = and (midCheck (NodeM v k tl tr) : func tl : [func tr])
         | tl /= EmptyM = and (midCheck (NodeM v k tl tr) : [func tl])
         | tr /= EmptyM = and (midCheck (NodeM v k tl tr) : [func tr])
         | otherwise = True
      func EmptyM = False
      check (NodeM _ k tr tl) = (tr /= EmptyM && tl /= EmptyM) || k /= 0
      check EmptyM = False

-- ������ 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch tre el = if isSearch tre then func tre else False
   where 
      func (NodeM v k tr tl)
         | el == v = True
         | tl == EmptyM && tr == EmptyM = False
         | otherwise = func (route (NodeM v k tr tl) el)
      func EmptyM = False

-- ������ 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch tree el = if elemSearch tree el then chTr tree (getTr tree el) (+) else newPl tree el

-- ������ 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a  -> BinTreeM a
delSearch tree el = if elemSearch tree el then chTr tree (getTr tree el) (-) else tree

-- ������ 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xr = if xr /= [] then inorder $ buildTr xr else []


-- ������ 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform tree = BInform (heightBtr tree) (smallest tree) (biggest tree)

-- ������ 7 ------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree num (NodeB xr tr) = checkRoot && checkNodes && check && allSorted (NodeB xr tr) && isAllRight (NodeB xr tr)
   where 
      check = length xr == length tr - 1
      checkRoot = 1 <= length xr && length xr <= (2*num - 1)
      checkNodes = num - 1 <= smalKeyAm (NodeB xr tr)  && bigKeyAm (NodeB xr tr) <= (2*num - 1)

-- ������ 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Btree a -> Btree a -> Bool 
eqBtree tree tree1 = sort (getAllKeys tree) == sort (getAllKeys tree1)
   

-- ������ 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tree el = elem el (getAllKeys tree)

position :: Ord a => a -> [a] -> Int
position = undefined

-- ������ 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree ord (NodeB xr1 tr1) el = if tr1 == [] then (NodeB (insertElem xr1 el) tr1) else  func (NodeB xr1 tr1) (0::Int)
   where
      func (NodeB xr tr) num
         | tr /= [] && length xr == (ord * 2 - 1) = func (head(drop (findPosSingle xrreb el) trreb)) (num+1)
         | tr /= [] && length xr /= (ord * 2 - 1) = func (head(drop (findPosSingle xr el) tr)) (num + 1)
         | length xr == (ord * 2 - 1) = if num < 2 then test2 ord (NodeB xr1 tr1) (NodeB xr tr) el else fullInsert (NodeB xr1 tr1) (findFath (NodeB xr1 tr1) (NodeB xr tr)) (test2 ord (NodeB xr1 tr1) (NodeB xr tr) el)
         | otherwise =  if num < 2 then test2 ord (NodeB xr1 tr1) (NodeB xr tr) el else fullInsert (NodeB xr1 tr1) (findFath (NodeB xr1 tr1) (NodeB xr tr)) (test2 ord (NodeB xr1 tr1) (NodeB xr tr) el)
         where
            (NodeB xrreb trreb) = test3 ord (NodeB xr1 tr1) (NodeB xr tr) 

isFull :: Ord a => Int -> Btree a -> Bool
isFull = undefined

insertKey :: Ord a => a -> [a] -> [a]
insertKey = undefined

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> 
                        ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB = undefined

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB = undefined

-----------------------

bm :: BinTreeM Char
bm = NodeM  'g' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            )
            (NodeM 'w' 2  EmptyM EmptyM)   


bm1 :: BinTreeM Char
bm1 = NodeM  'z' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)  


bm2 :: BinTreeM Char
bm2 = NodeM  'c' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'b' 2  EmptyM EmptyM)


bm3 :: BinTreeM Int
bm3 = NodeM 8 2  
            (NodeM 3 1  EmptyM 
                    (NodeM 5 1 
                             (NodeM 4 2 EmptyM EmptyM)
                             (NodeM 7 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 9 2  EmptyM EmptyM)


bm4 :: BinTreeM Int
bm4 = NodeM 3 2  
            (NodeM 2 1  EmptyM 
                    (NodeM 6 1 
                             (NodeM 4 2 EmptyM EmptyM)
                             (NodeM 7 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 5 2  (NodeM 3 2 EmptyM EmptyM)
                        (NodeM 7 1 (NodeM 10 2 EmptyM EmptyM)
                                    (NodeM 8 1 EmptyM EmptyM)))


bm5 :: BinTreeM Char
bm5 = NodeM 'g' 2  
            (NodeM 'c' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'l' 2  (NodeM 'i' 2 EmptyM EmptyM)
                        (NodeM 'n' 1 (NodeM 'm' 2 EmptyM EmptyM)
                                    (NodeM 'q' 1 EmptyM EmptyM)))


bm6 :: BinTreeM Char
bm6 = NodeM 'c' 2  
            (NodeM 'b' 1  EmptyM 
                    (NodeM 'g' 1 
                             (NodeM 'e' 2 EmptyM EmptyM)
                             (NodeM 'h' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'l' 2  (NodeM 'f' 2 EmptyM EmptyM)
                        (NodeM 'r' 1 (NodeM 'a' 2 EmptyM EmptyM)
                                    (NodeM 'l' 1 EmptyM EmptyM)))

bm7 :: BinTreeM Char
bm7 = NodeM 'C' 1 EmptyM (NodeM 'a' 4 EmptyM (NodeM 'p' 1 (NodeM 'b' 1 EmptyM (NodeM 'l' 1 (NodeM 'c' 1 EmptyM EmptyM) (NodeM 'n' 1 EmptyM EmptyM))) EmptyM))

bm8:: BinTreeM Int
bm8 = NodeM 12 1 (NodeM 10 2 (NodeM 8 1 (NodeM 7 1 EmptyM EmptyM) (NodeM 9 1 EmptyM EmptyM)) (NodeM 11 1 EmptyM EmptyM)) (NodeM 15 1 (NodeM 14 1 EmptyM EmptyM) (NodeM 17 1 (NodeM 16 1 EmptyM EmptyM) (NodeM 19 1 EmptyM EmptyM)))

tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX" [NodeB "ABCDE" [],NodeB "JK" [],NodeB "NO" [],NodeB "QRS" [],NodeB "UV" [],NodeB "YZ" []]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM" [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" [] ]
       , NodeB "TX" [NodeB "QRS" [],NodeB "UV" [],NodeB "YZ" []]
       ]
tBt8t :: Btree Char
tBt8t = NodeB "GM" [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

--------------------------------------------------------------------
-- f:: String -> (Maybe a, String)
-- f - analyser
-- Parser a - type
-- parse - селектор, що вибирає аналізатор f

newtype Parser a = Parser {parse:: String -> (Maybe a, String)}

item:: Parser Char
item = Parser(\s -> case s of 
                     ""     -> (Nothing, "")
                     (c:cs) -> (Just c, cs))
-- item теж аналізатор
-- parse витягає значення з нього

--------- idk
-- (<|>) :: Parser a -> Parser a -> Parser adj
-- p <|> q = Parser(\s -> case parse p s of
--                            (Nothing, _) -> parse q s
--                            res          -> res)

-- sign:: Parser String
-- sign = string '-' <|> return []

------------------------------------------------------------------