{-# OPTIONS_GHC -Wall #-}
module Bohuta10 where

     import Text.ParserCombinators.Parsec

     data Value = I Int  | B Bool deriving (Show, Eq)
     data Exp = Var String      -- Змінна
          | Const Value     -- константа
          | Op Exp Bop Exp  -- Операція
                    deriving (Show, Eq)
     -- Бінарні (2-аргумента) оператори
     data Bop =  Plus | Minus | Times | Div
               | Gt | Ge | Lt | Le| Eql | And | Or
               deriving (Show, Eq)

     data Stmt = Assign String Exp
               | Read String
               | Write Exp
               | Incr String
               | If Exp Stmt
               | While Exp Stmt
               | For Stmt Exp Stmt Stmt
               | Block [(String,Type)] [Stmt]
               deriving (Show, Eq)
     data Type = It | Bt deriving (Show, Eq)
     type Program = Stmt

     type StateW = ([String], [(String,Value)], [String])

     type VarEnv  = [(String,Type)]

     getState1:: StateW -> [String]
     getState1 s = case s of (a,_,_) -> a
     getState2:: StateW -> [(String, Value)]
     getState2 s = case s of (_,a,_) -> a
     getState3:: StateW -> [String]
     getState3 s = case s of (_,_,a) -> a
     -- Задача 1.a -----------------------------------------
     getValue::  StateW -> String -> Value
     getValue st v = head [snd i|i <- getState2 st, fst i == v]

     -- Задача 1.b -----------------------------------------
     updValue :: StateW -> String -> Value -> StateW
     updValue st d v = (getState1 st,[if fst i == d then (fst i, v) else i|i<-getState2 st],getState3 st)

     -- Задача 2 ----------------------------------------- 
     readValue :: StateW -> Type -> (StateW,Value)
     readValue st t
          | t == It = case checkInt (head (getState1 st)) of
               Just a -> ((tail (getState1 st), getState2 st, getState3 st), I a)
               _ -> (st, I 0)
          | otherwise = case checkBool (head (getState1 st)) of
               Just a -> ((tail (getState1 st), getState2 st, getState3 st), B a)
               _ -> (st, B False )

     checkInt::String -> Maybe Int
     checkInt s
          | s=="True" || s == "False" = Nothing
          | otherwise = Just (read s::Int)

     checkBool::String -> Maybe Bool
     checkBool s
          | s=="True" || s == "False" = Just (read s:: Bool)
          |otherwise = Nothing

     -- Задача 3 -----------------------------------------
     writeValue :: StateW -> Value -> StateW
     writeValue st (I v) = (getState1 st, getState2 st, getState3 st ++ [show v])
     writeValue st (B v) = (getState1 st, getState2 st, getState3 st ++ [show v])

     -- Задача 4.a ----------------------------------------- 
     appOp:: Bop -> Value -> Value -> Value
     appOp op (I v1) (I v2) = case op of
          Plus -> I (v1 + v2)
          Minus -> I (v1 - v2)
          Times -> I (v1*v2)
          Div -> I (div v1 v2)
          Gt -> B (v1 > v2)
          Ge -> B (v1>=v2)
          Lt -> B (v1 < v2)
          Le -> B (v1<=v2)
          Eql -> B (v1==v2)
          _ -> undefined
     appOp op (B v1) (B v2) = case op of
          Eql -> B (v1==v2)
          And -> B (v1&&v2)
          Or -> B(v1||v2)
          _ -> undefined
     appOp _ _ _ = undefined

     evExp :: StateW -> Exp -> Value
     evExp st (Var a) = if null a then B False else getValue st a
     evExp _ (Const a) = a
     evExp st (Op e1 op e2) = appOp op (evExp st e1) (evExp st e2)

     -- Задача 4.b -----------------------------------------
     isInState :: StateW -> String -> Bool
     isInState st s = not (null [s|i<- getState2 st, fst i == s])

     addToState :: StateW -> String -> Value -> StateW
     addToState st s v = (getState1 st, getState2 st ++ [(s,v)], getState3 st)

     evStmt :: StateW -> Stmt -> StateW
     evStmt st (Assign s ex)= if isInState st s then updValue st s (evExp st ex) else addToState st s (evExp st ex)
     evStmt st (Read s)
          | fst (readValue st It) == st = case readValue st Bt of
               (st1,v) -> if isInState st1 s then updValue st1 s v else addToState st1 s v
          | otherwise = case readValue st It of
               (st1,v) -> if isInState st1 s then updValue st1 s v else addToState st1 s v
     evStmt st (Write e) = writeValue st (evExp st e)
     evStmt st (Incr s) = (getState1 st, [if fst i == s then (s,appOp Plus (snd i) (I 1)) else i|i<-getState2 st], getState3 st)
     evStmt st (If e stmt1)
          | evExp st e == B True = evStmt st stmt1
          | otherwise = st
     evStmt st (While e stmt2)
          | evExp st e == B True = evStmt (evStmt st stmt2) (While e stmt2)
          | otherwise = st
     evStmt st (For stmt1 e stmt2 stmt3) = if evExp (evStmt st stmt1) e == B True then evStmt (evStmt (evStmt (evStmt st stmt1) stmt3) stmt2) (For (Assign [] (Var "")) e stmt2 stmt3) else st
     evStmt st (Block vs stmts) = foldl evStmt (pushVals vs st) stmts

     pushVals :: [(String, Type)] -> StateW -> StateW
     pushVals ss (i, s, o) = (i,map helpF ss++s, o)

     helpF :: (String, Type) -> (String, Value)
     helpF (s, It) = (s, I 0)
     helpF (s, Bt) = (s, B False)

     -- Задача 4.c -----------------------------------------
     evProgram :: Program -> [String] -> [String]
     evProgram p ls = getState3 (evStmt (ls, [], []) p)

     ---- Перевірка контекстних умов -----------------------
     -- Задача 5.a -----------------------------------------
     isApply :: Bop -> Type -> Type -> Maybe Type
     isApply Plus It It = Just It
     isApply Minus It It = Just It
     isApply Times It It = Just It
     isApply Div It It = Just It
     isApply Gt It It = Just Bt
     isApply Ge It It = Just Bt
     isApply Lt It It = Just Bt
     isApply Le It It = Just Bt
     isApply Eql It It = Just Bt
     isApply Eql Bt Bt = Just Bt
     isApply And Bt Bt = Just Bt
     isApply Or Bt Bt = Just Bt
     isApply _ _ _ = Nothing

     iswfOp :: Bop -> [Type] -> Maybe Type
     iswfOp op tps
          | length tps /= 2 = Nothing
          | otherwise = isApply op (head tps) (last tps)

     -- Задача 5.b -----------------------------------------
     isInVarEnv :: VarEnv -> String -> Bool
     isInVarEnv ve s = not (null [s|i <- ve, fst i == s])

     getValueFromVarEnv :: VarEnv -> String -> Type
     getValueFromVarEnv ve s = head [snd i|i<-ve, fst i == s]

     iswfExp :: Exp -> VarEnv -> Maybe Type
     iswfExp (Var v) ve
          | isInVarEnv ve v = Just (getValueFromVarEnv ve v)
          | otherwise = Nothing
     iswfExp (Const v) _ = case v of
          I _ -> Just It
          B _ -> Just Bt
     iswfExp (Op e1 op e2) ve = case iswfExp e1 ve of
          Just a -> case iswfExp e2 ve of
               Just b -> iswfOp op [a, b]
               Nothing -> Nothing
          Nothing -> Nothing

     -- Задача 5.c -----------------------------------------
     checkExp::Maybe Type -> Bool
     checkExp mt = case mt of
          Just _ -> True
          _ -> False

     iswfStmt :: Stmt -> VarEnv -> Bool
     iswfStmt (Assign _ ex) ve = checkExp (iswfExp ex ve)
     iswfStmt (Read s) ve = isInVarEnv ve s
     iswfStmt (Write ex) ve = checkExp (iswfExp ex ve)
     iswfStmt (Incr s) ve = case iswfExp (Var s) ve of
          Just It -> True
          _ -> False
     iswfStmt (If ex stm) ve = iswfExp ex ve == Just Bt && iswfStmt stm ve
     iswfStmt (While ex stm) ve = iswfExp ex ve == Just Bt && iswfStmt stm ve
     iswfStmt (For stm1 ex stm2 stm3) ve = iswfStmt stm1 ve && iswfExp ex ve == Just Bt &&  iswfStmt stm2 ve &&  iswfStmt stm3 ve
     iswfStmt (Block vs stms) ve = and [iswfStmt i (ve++vs)|i<-stms]

     iswfProgram :: Program -> Bool
     iswfProgram st = iswfStmt st []

     ---- Синтаксичний аналіз -------
     iden :: Parser String
     iden = try( do {nm <- identif;
                    if (any(nm==) ["int","bool","read","write","if","while","for","true","false"])
                         then unexpected ("reserved word " ++ show nm)
                         else return nm
                    } )

     oper  :: String -> Bop -> Parser Bop
     oper str bop = do {_ <- string str; return bop}

     mulOp :: Parser Bop
     mulOp = (oper "*" Times) <|> (oper "/" Div)

     disOp :: Parser Bop
     disOp = (oper "&" And)

     conOp :: Parser Bop
     conOp = (oper "|" Or)

     --розпізнавати ВСІ порожні символи в кінці
     lexem :: Parser a -> Parser a
     lexem p = do {a <- p; spaces; return a}

     --   :type Op -----> Exp -> Bop -> Exp -> Exp 
     --   :type flip Op -------> Bop -> Exp -> Exp -> Exp         
     expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
     expOp p = do {x <- lexem p; return (flip Op x)}

     symbol :: Char ->  Parser ()
     symbol ch = lexem (char ch >> return ())

     keyword :: String -> Parser ()
     keyword st = try( lexem( string st >> notFollowedBy alphaNum))

     typev :: Parser Type
     typev = do {keyword "int"; return It}
          <|> do {keyword "bool"; return Bt}

     -- Задача 6.a -----------------------------------------
     identif :: Parser String
     identif = do{a <- letter; b<-many (letter <|> digit); return (a:b)}

     number :: Parser Int
     number  = do {a <- digit; b <- many digit; return (read (a:b))}

     addOp :: Parser Bop
     addOp = do{symbol '+'; return Plus} <|> do{symbol '-'; return Minus}

     rh1 :: Parser Bop
     rh1 = do {symbol '='; return Ge}

     rh2 :: Parser Bop
     rh2 = do {symbol '='; return Le}

     relOp :: Parser Bop
     relOp =
          do {symbol '>'; rh1 <|> return Gt} <|>
          do{symbol '<'; rh2 <|> return Lt} <|>
          do {symbol '='; symbol '='; return Eql} 
          -- do{symbol '<'; return Lt} <|>
          -- do {symbol '>'; return Gt}

     -------------------------------------------------------
     factor :: Parser Exp
     factor = do { symbol '('; x <- expr; symbol ')'; return x}
          <|> do {nm <- lexem number; return (Const (I nm))}
          <|> do {keyword "true"; return (Const (B True))}
          <|> do {keyword "false"; return (Const (B False))}
          <|> do {cs <- lexem iden; return (Var cs) }
          <?> "factor"

     -- Задача 6.b -----------------------------------------
     buildRec :: Exp -> [(Bop, Exp)] -> Exp
     buildRec a [] = a
     buildRec e [a] = Op e (fst a) (snd a)
     buildRec e l = buildRec (Op e (fst (head l)) (snd (head l))) (tail l)

     ht :: Parser (Bop, Exp)
     ht = do{a <- mulOp; b<-factor; return (a,b)}

     term :: Parser Exp
     term = do {e1 <- factor; a <- many ht; return (buildRec e1 a)}

     hr :: Parser (Bop, Exp)
     hr = do{a <- addOp; b<-term; return (a,b)}

     relat :: Parser Exp
     relat = do{e1 <- term; a <- many hr; return (buildRec e1 a) }

     hc :: Parser (Bop, Exp)
     hc = do{a <- relOp; b <- relat; return (a,b)}

     conj :: Parser Exp
     conj = do{e1 <- relat; a <- many hc; return  (buildRec e1 a)}

     hd :: Parser (Bop, Exp)
     hd = do{a <- conOp; b <- conj; return (a,b)}

     disj :: Parser Exp
     disj = do {e1 <- conj; a<- many hd; return  (buildRec e1 a)}

     he :: Parser (Bop, Exp)
     he = do{a <- disOp; b <- disj; return (a,b)}

     expr :: Parser Exp
     expr = do {e1 <- disj; a <- many he; return  (buildRec e1 a)}

     ------------------------------------------------------
     stmt :: Parser Stmt
     stmt = do {keyword "for"; forSt}
          <|> do {keyword "while"; whileSt}
          <|> do {keyword "if"; ifSt}
          <|> do {keyword "read"; inSt}
          <|> do {keyword "write"; outSt}
          <|> do {var <- lexem iden; assignSt var}
          <|> blockSt
          <?> "statement"

     -- Задача 6.c -----------------------------------------

     forSt :: Parser Stmt
     forSt   = do{symbol '('; stm1 <- stmt; symbol ';'; e <- expr; symbol ';'; stm2 <- stmt; symbol ')'; stm3 <- stmt; return (For stm1 e stm2 stm3)}

     whileSt :: Parser Stmt
     whileSt = do{symbol '('; e <- expr; symbol ')'; stm <- stmt; return (While e stm)}

     ifSt :: Parser Stmt
     ifSt    = do {symbol '('; e <- expr; symbol ')';stm <- stmt; return (If e stm)}

     inSt :: Parser Stmt
     inSt    = do{s <- iden; return (Read s)}

     outSt :: Parser Stmt
     outSt    = do{e <- expr; return (Write e)}

     assignSt :: String -> Parser Stmt
     assignSt s = do {symbol '+'; symbol '+'; return (Incr s)} <|>
          do {symbol ':'; symbol '=';e <- expr; return (Assign s e)}

     hListId :: Parser String
     hListId = do {symbol ',';s <- iden; return s}

     listId :: Parser [String]
     listId = do {a <- iden; la <- many hListId; return (a:la)}

     hListSt :: Parser Stmt
     hListSt = do{symbol ';'; s <- stmt; return s}

     listSt :: Parser [Stmt]
     listSt = do {a <- stmt; al <- many hListSt; return (a:al)}

     mapDefin :: Type -> [String] -> [(String , Type)]
     mapDefin t sl = [(i, t)|i <- sl]

     defin :: Parser [(String, Type)]
     defin = do{a <- typev; b <- listId; symbol ';'; return (mapDefin a b)}

     mapArrDefin :: [[(String, Type)]] -> [(String , Type)]
     mapArrDefin ar = [q|i <- ar, q <- i]

     blockSt :: Parser Stmt
     blockSt = do {symbol '{';al <- many defin; st <- listSt; symbol '}';return (Block (mapArrDefin al) st) }
     -- blockSt = undefined 

     ---------------------------------------------	
     -- Головні функції
     ---------------------------------------------				
     program :: Parser Stmt
     program = do {spaces; r <- stmt; eof; return r}

     parseL :: String -> Either ParseError Program
     parseL s = parse program "" s

     -- Програми -------------------------------------------
     squareRoot :: Program
     squareRoot = Block [("a",It),("b",It)]
                    [ Read "a", Assign "b" (Const (I 0))
                    , If (Op (Var "b") Ge (Const(I 0)))
                         (Block [("c", Bt)]
                                   [Assign "c" (Const (B True)),
                                   While (Var "c")
                                   (Block []
                                        [(Incr "b"),
                                        Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                        ])
                                   ]
                         )
                    , Write (Op (Var "b") Minus (Const (I 1)))
                    ]

     squareRootS :: String
     squareRootS =
          "{int a, b; \
          \ read a; b := 0; \
          \ if (a>= 0)\
          \    {bool c; c:=true; while(c) {b++; c:= a >= b*b}\
          \    };\
          \  write (b-1)\
          \ }"

     fibonacci :: Program
     fibonacci =
          Block [("in",It), ("out",It)]
                    [ Read "in",  Assign "out" (Const (I 0))
                    , If (Op (Var "in") Gt (Const(I 1)))
                         (Block [("f0",It), ("f1",It), ("c",It)]
                              [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                              If (Op (Var "in") Gt (Const (I 1)))
                                   (For (Assign "c" (Const (I 1)))
                                   (Op (Var "c") Lt (Var "in"))
                                   (Incr "c")
                                   (Block []
                                             [Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                             , Assign "f0" (Var "f1")
                                             , Assign "f1" (Var "out")
                                             ]
                                        )
                                   )
                              ])
                    , Write (Var "out")
                    ]

     fibonacciS :: String
     fibonacciS =
          " {int in, out; read in; out := 0; \n\
          \if (in>=0){int f0, f1,c; \n\
          \           f0 := 1; f1 := 1; out := 1; \n\
          \           if(in>1) \n \
          \              for (c := 1; c < in; c++) {\n\
          \                   out := f0 + f1; f0 := f1; f1 := out\n\
          \              }\n\
          \          }; \n\
          \write out \n\
          \}"

