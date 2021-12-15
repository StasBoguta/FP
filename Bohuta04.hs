{-# OPTIONS_GHC -Wall #-}
module Bohuta04 where

  import Data.List(nub,sort)

  -- машина Тюрінга
  data Going = No | Lt | Rt deriving (Show,Eq,Ord)
  type Table = [((Int,Char),(Int,Char,Going))]
  type Machine = (Int, Table)
  type Config = (String,(Int,Char), String, Int)

  -- опис складної машини Тюрінга 
  -- L | R | P Char | G  базові машини
  data Complex = Join [Complex]
              | While Char Complex
              | If Char Complex Complex
              | P Char
              | L | R | G
              deriving (Show, Eq)

  get1Config :: Config -> String
  get1Config c = case c of (a,_,_,_) -> a
  get2Config :: Config -> (Int, Char)
  get2Config c = case c of (_,a,_,_) -> a
  get3Config :: Config -> String
  get3Config c = case c of (_,_,a,_) -> a
  get4Config :: Config -> Int
  get4Config c = case c of (_,_,_,a) -> a

  get1Table :: (Int, Char, Going) -> Int
  get1Table t = case t of (a,_,_) -> a
  get2Table :: (Int, Char, Going) -> Char
  get2Table t = case t of (_,a,_) -> a
  get3Table :: (Int, Char, Going) -> Going
  get3Table t = case t of (_,_,a) -> a

  -- Задача 1.a -----------------------------------------
  alphabet :: Table -> String
  alphabet t = nub (sort ([snd (fst i)| i <- t] ++ [ case snd i of (_, a, _) -> a  |i <- t]))

  -- Задача 1.b ----------------------------------------- 
  states :: Machine -> [Int]
  states m = nub (sort [fst (fst i)| i <- snd m])

  -- Задача 2 -----------------------------------------
  isAllStates :: Machine -> Bool
  isAllStates m = and [i `elem` states m| i<- [1.. fst m]]

  getSteps :: Table -> [(Int, Char)]
  getSteps t = [fst i| i <- t]

  isFullAlphabet :: Machine -> Bool
  isFullAlphabet m = and [(q, i) `elem` getSteps (snd m)|i <- alphabet (snd m), q <- states m]

  iswfMachine :: Machine -> Bool
  iswfMachine m = isAllStates m && isFullAlphabet m

  initCon :: Machine -> String -> Config
  -- будує початкову конфігурацію за машиною і вхідним рядком 
  initCon (is,_) ""     = ("", (is,' '), "", 0)
  initCon (is,_) (c:cx) = ("", (is, c), cx, 0)

  -- Задача 3.a -----------------------------------------
  isFinal ::  Int -> Config -> Bool
  isFinal i c = (case c of (_,_,_,a) -> a) == i || fst (case c of (_,a,_,_) -> a) == 0

  -- Задача 3.b -----------------------------------------
  filterTableByState :: Table -> (Int, Char) -> ((Int,Char),(Int,Char,Going))
  filterTableByState t state = head (filter (\e -> state == fst e) t)

  getMovement :: Machine->Config->Going
  getMovement m c = get3Table (snd (filterTableByState (snd m) (get2Config c)))

  getTableStateByConfig :: Machine -> Config -> Int
  getTableStateByConfig m c = get1Table (snd (filterTableByState (snd m) (get2Config c)))

  getTableCharByConfig :: Machine -> Config -> Char
  getTableCharByConfig m c = get2Table (snd (filterTableByState (snd m) (get2Config c)))

  moveLeft::Machine->Config->Config
  moveLeft m c = (if null (get1Config c) then "" else init (get1Config c),
    (getTableStateByConfig m c, if null (get1Config c) then ' ' else last (get1Config c)),
    if get3Config c == " " && getCharFromModified m c == ' ' then [] else getCharFromModified m c : checkEmptyString (get3Config c) ,
     get4Config c + 1)

  getCharFromModified :: Machine -> Config -> Char
  getCharFromModified m c = get2Table (snd (filterTableByState (snd m) (get2Config c)))

  moveRight::Machine->Config->Config
  moveRight m c = (if get1Config c == " " && getCharFromModified m c == ' ' then [] else checkEmptyString (get1Config c) ++ [getCharFromModified m c],(getTableStateByConfig m c, if null (get3Config c) then ' ' else head (get3Config c)), if null (get3Config c) then get3Config c else tail (get3Config c), get4Config c + 1)

  notMove::Machine->Config->Config
  notMove m c = (get1Config c, (getTableStateByConfig m c, getTableCharByConfig m c), get3Config c, get4Config c + 1)

  stepM :: Machine -> Config -> Config
  stepM m c
    | getMovement m c == Rt = moveRight m c
    | getMovement m c == Lt = moveLeft m c
    | otherwise = notMove m c

  -- Задача 4 -----------------------------------------

  checkEmptyString :: String -> String
  checkEmptyString s = if s == " " then [] else s

  checkEmptyChar :: Char -> String
  checkEmptyChar ch = if ch == ' ' then [] else [ch]

  isEndConfig :: (Int, Char) -> Bool
  isEndConfig (a,_) = a == 0

  calculate :: Machine -> Config -> Int -> Maybe String
  calculate m c end
    | get4Config c == end && isEndConfig (get2Config c) == False = Nothing
    | isEndConfig (get2Config c) = Just (if (get1Config c == " " || get1Config c ==[]) && snd (get2Config c) == ' ' then if get3Config c == " " then [] else get3Config c else (checkEmptyString (get1Config c) ++  (if snd (get2Config c) == ' ' && (get3Config c == " "||get3Config c == []) then [] else ([snd (get2Config c)] ++ checkEmptyString (get3Config c)))))
    | otherwise = calculate m (stepM m c) end

  eval :: Machine -> Int -> String -> Maybe String
  eval m i s = calculate m (initCon m s) i

  -- Задача 5.a -----------------------------------------
  incrementTable::Int -> (Int,Char,Going) -> (Int,Char,Going)
  incrementTable i (a,b,c) = if a==0 then (a,b,c) else (a+i,b,c)

  incrementState::Int -> (Int,Char) -> (Int,Char)
  incrementState i (a,b) = if a==0 then (a,b) else (a+i,b)

  renum :: Int -> Machine -> Machine
  renum i m = (fst m + i, [(incrementState i (fst q), incrementTable i (snd q))| q <- snd m])

  -- Задача 5.b -----------------------------------------
  changeState :: Int -> (Int,Char,Going) -> (Int,Char,Going)
  changeState i (a,b,c) = if a==0 then (i,b,c) else (a,b,c)

  connect :: Int -> Table -> Table
  connect i t = [(fst q, changeState i (snd q))|q <- t]

  -- Задача 6.a -----------------------------------------
  seqJoin :: Machine -> Machine -> Machine
  seqJoin m1 m2 = (fst (renum (fst m2) m1), snd m2 ++ connect (fst m2) (snd (renum (fst m2) m1)) )

  -- Задача 6.b -----------------------------------------
  ifJoin :: Char -> String -> Machine -> Machine -> Machine
  ifJoin c st m1 m2 = (fst (renum (fst m2) m1) + 1, snd m2 ++ snd (renum (fst m2) m1) ++ [ ((fst (renum (fst m2) m1) + 1,s), (if s==c then fst (renum (fst m2) m1) else fst m2,s,No))| s <- st ])

  -- Задача 6.c -----------------------------------------
  cycleJoin :: Char -> String -> Machine -> Machine
  cycleJoin c st m = (fst m + 1, connect (fst m + 1) (snd m) ++ [ ((fst m + 1,s), (if s==c then fst m else 0,s,No))  | s<-st ])

  -- Задача 7 -----------------------------------------
  build ::  String -> Complex -> Machine
  build s (While c m) = cycleJoin c s (build s m)
  build s (Join c) = if length c == 1 then build s (head c) else seqJoin (build s (head c)) (build s (Join (tail c)))
  build s (If ch c1 c2) = ifJoin ch s (build s c1) (build s c2)
  build s R = (1, map (\c->((1,c),(0,c,Rt))) s)
  build s (P c1) = (1, map (\c->((1,c),(0,c1,No))) s)
  build s L = (1, map (\c->((1,c),(0,c,Lt))) s)
  build s G = (1, map (\c->((1,c),(0,c,No))) s)

  -- Задача 8.a-----------------------------------------
  subtractAbs :: Complex
  subtractAbs = Join[
      While '|' (Join[P ' ',R, fullRight, L, If '#' (Join[P '|', R]) (Join [P ' ', L, If '#' (P ' ') (Join[P ' ', L, fullLeft, R, If '#' (Join[P '|', L]) (Join[P ' ', R])])])]) 
    , If '#' (P ' ') G]

  -- Задача 8.b-----------------------------------------
  subtraction :: Complex
  

  --------------------------------------------------------
  --  тестові дані 
  -- приклади машин Тюрінга 
  test1, test2 :: Machine
  -- алфавіт " abc": знаходить перший символ 'a' заміняє на 'b' і зупиняється  
  test1 = (1, [ ((1,'a'),(0,'b',No)), ((1,'b'),(1,'b',Rt))
              , ((1,'c'),(1,'c',Rt)), ((1,' '),(1,' ',Rt))])
  -- алфавіт " a": невизначена функція переходу в (1,'a') !!! 
  test2 = (2,[((2,'a'),(2,'a',Rt)),((2,' '),(1,' ',Rt)),((1,' '),(0,' ',Rt))])

  -- будуємо складну машину з найпростіших
  -- будуємо машину, що обчислює додавання
  -- найпростіші . алфавіт == " #|"
  rht, putO, putW :: Machine
  rht  = (1, map (\c->((1,c),(0,c,Rt))) " #|")   -- переміщує вправо
  putO = (1, map (\c->((1,c),(0,'|',No))) " #|") -- записує символ '|'
  putW = (1, map (\c->((1,c),(0,' ',No))) " #|") -- записує символ ' '

  -- складніші машини 
  rightO, rightM, main, additionM :: Machine
  rightO = cycleJoin '|' " #|" rht      -- проходить вправо всі '|'
  rightM = seqJoin rht rightO           -- вправо завжди і потім вправо всі '|' 
  main   = seqJoin (seqJoin putW rightM) putO  -- додавання, коли x>0
  additionM = ifJoin '|' " #|" main putW       -- додавання, коли x>=0 

  -- приклади побудов машин Тюрінга (обєкти типу Complex)
  right, left, copy, addition, fullRight, fullLeft :: Complex
  -- вправо завжди і потім вправо всі '|'
  right = Join [R,While '|' R]
  fullRight = Join[While '|' R, While '#' R, While '|' R]
  fullLeft = Join[While '|' L, While '#' L, While '|' L] 
  subtraction = If '#' (Join[R, If '|' (While '|' G) (Join[L, P ' '])]) (While '|' (Join[P ' ', R, fullRight, L, If '#' (Join[P '|', R]) (Join[P ' ', L, If '#' (Join[P ' ', G]) (Join[P ' ', L, fullLeft, R, If '|' (Join[P ' ', R, If '#' (Join[R, If '|' (While '|' G) (Join [L, P ' '])]) (P '|')]) (While '#' G) ])])]))
  -- вліво завжди і потім вліво всі '|'  
  left  = Join [L,While '|' L]
  -- додавання x+y 
  
  addition = If '|' (Join [P ' ',right,P '|']) (P ' ')
  -- копіювання *|.x.| |.y.| ==> *|.x.| |.y+x.| 
  copy = Join [While '|' (Join [P ' ',right,right,P '|',left,left,P '|',R]),Join [left,R]]

  rightOT, rightMT, mainT, additionMT :: Machine
  rightOT = (2,
    [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
    ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))])
  rightMT = (3,
    [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
    ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))
    ,((3,' '),(2,' ',Rt)),((3,'#'),(2,'#',Rt)),((3,'|'),(2,'|',Rt))])
  mainT = (5,
    [((1,' '),(0,'|',No)),((1,'#'),(0,'|',No)),((1,'|'),(0,'|',No))
    ,((2,' '),(3,' ',Rt)),((2,'#'),(3,'#',Rt)),((2,'|'),(3,'|',Rt))
    ,((3,' '),(1,' ',No)),((3,'#'),(1,'#',No)),((3,'|'),(2,'|',No))
    ,((4,' '),(3,' ',Rt)),((4,'#'),(3,'#',Rt)),((4,'|'),(3,'|',Rt))
    ,((5,' '),(4,' ',No)),((5,'#'),(4,' ',No)),((5,'|'),(4,' ',No))])
  additionMT = (7,
    [((1,' '),(0,' ',No)),((1,'#'),(0,' ',No)),((1,'|'),(0,' ',No))
    ,((2,' '),(0,'|',No)),((2,'#'),(0,'|',No)),((2,'|'),(0,'|',No))
    ,((3,' '),(4,' ',Rt)),((3,'#'),(4,'#',Rt)),((3,'|'),(4,'|',Rt))
    ,((4,' '),(2,' ',No)),((4,'#'),(2,'#',No)),((4,'|'),(3,'|',No))
    ,((5,' '),(4,' ',Rt)),((5,'#'),(4,'#',Rt)),((5,'|'),(4,'|',Rt))
    ,((6,' '),(5,' ',No)),((6,'#'),(5,' ',No)),((6,'|'),(5,' ',No))
    ,((7,' '),(1,' ',No)),((7,'#'),(1,'#',No)),((7,'|'),(6,'|',No))])