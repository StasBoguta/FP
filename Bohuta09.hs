{-# OPTIONS_GHC -Wall #-}
module Bohuta09 where

        import Text.ParserCombinators.Parsec
        import Data.Char (isDigit)

        data Term   =  Nmb Int         -- десяткове число без знаку
                | Var String       -- змінна, довільний ідентифікатор
                | App Term Term    -- операція застосування
                | Abs String Term  --  операція абстракції
                deriving (Show, Eq)
        type Contex = [(String,Term)]


        -- Задача 1.a -----------------------------------------
        addVar :: String -> [String] -> [String]
        addVar v vs = if null [i|i <- vs, i == v] then v:vs else vs

        -- Задача 1.b ----------------------------------------- 
        delVar :: String -> [String] -> [String]
        delVar v vs = [i|i <- vs, i /= v]

        -- Задача 1.c -----------------------------------------
        unionV :: [String] -> [String] -> [String]
        unionV a b = foldr addVar b a

        -- Задача 1.d ----------------------------------------- 
        freeVars :: Term -> [String]
        freeVars (App t1 t2) = unionV (freeVars t1) (freeVars t2)
        freeVars (Var s) = [s]
        freeVars (Abs s t) = delVar s (freeVars t)
        freeVars (Nmb _) = []

        -- Задача 2.a -----------------------------------------
        deleteSyn :: String -> Contex -> Contex
        deleteSyn s c = [i|i <- c, fst i /= s]

        -- Задача 2.b -----------------------------------------
        iswfTerm :: Term -> Contex -> Bool
        iswfTerm t cnt = null (freeVars t) || (length [tn| tn <- freeVars t, n <- cnt, tn == fst n] == length (freeVars t))

        -- Задача 2.c -----------------------------------------
        iswfContex :: Contex -> Bool
        iswfContex cnt = and [iswfTerm (snd (cnt!!i)) [cnt !! q|q <- [0.. i-1]] |i <- [0..length cnt -1]] && and [length (deleteSyn (fst i) cnt) == (length cnt - 1) |i <- cnt]

        -- Задача 3.a -----------------------------------------
        isNumber :: Term -> Bool
        isNumber t = null (freeVars t)  && helpNumber t

        helpNumber :: Term -> Bool
        helpNumber (Abs x (Abs y t)) = analyzeNumber [x, y] t
        helpNumber _ = False

        analyzeNumber :: [String] -> Term -> Bool
        analyzeNumber arr (Var x) = last arr == x
        analyzeNumber arr (App (Var s) t) = head arr == s && analyzeNumber arr t
        analyzeNumber _ _ = False

        -- Задача 3.b -----------------------------------------
        toN :: Term -> Int
        toN (Var _) = 0
        toN (App _ l) = 1 + toN l
        toN _ = undefined

        inNumber :: Term -> Term
        inNumber (Abs _ (Abs _ t)) = Nmb (toN t)
        inNumber _ = undefined

        -- Задача 3.c -----------------------------------------
        compress ::  Term -> Term
        compress (App t1 t2) = App (if isNumber t1 then inNumber t1 else compress t1) (if isNumber t2 then inNumber t2 else compress t2)
        compress (Abs s t1) = if isNumber (Abs s t1) then inNumber (Abs s t1) else Abs s (if isNumber t1 then inNumber t1 else compress t1)
        compress a = a

        -- Задача 4 -----------------------------------------
        reduce :: Term -> String -> Term -> Term
        reduce (Var t) x r = if t == x then r else Var t
        reduce (App t1 t2) x r = App (reduce t1 x r) (reduce t2 x r)
        reduce (Abs s t) x r
                | s == x = Abs s t
                | s /= x && delVar s (freeVars r) == freeVars r = Abs s (reduce t x r)
                | otherwise =  Abs (newVar (unionV (freeVars r) (freeVars t)) s) (reduce (reduce t s (Var (newVar (unionV (freeVars r) (freeVars t)) s))) x r)
        reduce (Nmb a) _ _ = Nmb a

        -- Задача 5 -----------------------------------------
        isInContex :: String -> Contex -> Bool
        isInContex s c = length [i|i<-c, fst i == s] == 1

        selectTerm :: String -> Contex -> Term
        selectTerm s c = head [snd i|i<-c, fst i == s]

        evalHelp::Term->Contex->Maybe Term
        evalHelp (App (Abs s t1) t2) _ = Just (reduce t1 s t2)
        evalHelp (App t1 t2) c = case evalHelp t1 c of
                        Just a -> if a /= t1 then Just (App a t2) else case evalHelp t2 c of
                                Just b -> Just (App t1 b)
                                _ -> Nothing
                        _ -> case evalHelp t2 c of
                                Just b -> Just (App t1 b)
                                _ -> Nothing
        evalHelp (Nmb n) _ = Just(integerTerm n)
        evalHelp (Abs s t) c = case evalHelp t (deleteSyn s c) of
                        Just a -> Just (Abs s a)
                        _ -> Nothing
        evalHelp (Var a) c
                        | isInContex a c = Just (selectTerm a c)
                        | otherwise = Just (Var a)

        evalStep :: Term -> Contex -> Maybe Term
        evalStep t c = case evalHelp t c of
                Just t1 -> if t == t1 then Nothing else Just t1
                _ -> Nothing


        -- Задача 6 -----------------------------------------
        eval :: Int -> Term -> Contex -> Maybe Term
        eval st t c = case evalCount st t c 0 of
                Just a -> Just (compress a)
                _ -> Nothing

        evalCount:: Int -> Term -> Contex -> Int -> Maybe Term
        evalCount st t c cc
                | cc < st = case evalStep t c of
                                        Just a -> evalCount st a c (cc+1)
                                        _ -> Just t
                | otherwise = Nothing

        -- Задача 7 -----------------------------------------
        parseTerm :: String -> Maybe Term
        parseTerm str =
                case parse pFull "" str of
                Right t -> Just t
                _  -> Nothing


        pExp :: Parser Term
        pExp = do pMltFun
                

        helpP :: String -> Parser ()
        helpP s = do {_ <- string s; spaces} 

        pFunc :: Parser Term
        pFunc = do {helpP "\\";pId}
                

        pId :: Parser Term
        pId = do{v <- letter;spaces;dot <- (helpP "." >> pExp) <|> pId;spaces;return (Abs [v] dot)}

        pFact :: Parser Term
        pFact = funInBr <|> pVar <|> pNum  <|> pFunc

        pNum :: Parser Term
        pNum = do{var <- many1 digit;spaces;return (Nmb (read var))}
                
        pMltFun :: Parser Term
        pMltFun = do{spaces;apps <- many1 pFact;spaces;return (foldl1 App apps)}
        
        pFull :: Parser Term
        pFull = do{_ <- spaces;term <- pExp;eof;return term}

        pVar :: Parser Term
        pVar = do{var <- many1 letter;spaces;return (Var var)}

        funInBr :: Parser Term
        funInBr = do{spaces;helpP "(";term <- pExp;helpP ")";return term}
                --------------------------------------------------------
        -- integerTerm - з числа в вираз
        integerTerm :: Int ->  Term
        integerTerm n  = (Abs "s" (Abs "z" (buildTerm n))) where
                buildTerm 0 = Var "z"
                buildTerm j = (App (Var "s") (buildTerm (j-1)))

        --  New Name -- якщо імя dddname, де ddd-цифри і n-буква, то початкове імя - name 
        -- якщо змінна ccc, то її нові імена 0ccc,...,9ccc,09ccc,...
        -- цифри на початку - це створення нового імені (problem name capture)
        newVar :: [String] -> String -> String
        newVar fvs nm = (until (\n -> notElem n fvs) next) (next nm) where   -- flip elem fvs
                next n@(c:_)| c=='9'    = '0':n
                next (c:cx) | isDigit c = (succ c):cx
                next n      = '0':n

        --------------------------------------------------------
        -- Тестові приклади
        term0, term0a, term1, term1a, term1b, term1c :: Term
        term0 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z"))))
        term0a = Abs "z" (App (Var "s") (App (Var "s") (Var "z")))
        term1 = Abs "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
        term1a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
        term1b = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
        term1c = Abs "y" (App (Var "x") (Var "y"))

        term2, term2a, termAnd, termTest :: Term
        term2 = App (App (Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
                        (Abs "x" (Abs "y" (Var "x")))
                )
                (Abs "x" (Abs "y" (Var "x")))
        term2a = App (Var "x") (App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Var "z")))
        termAnd = Abs "x" (Abs "y" (App (App  (Var "x") (Var "y")) (Var "false")))
        termTest = Abs "x" (Abs "x" (Abs "y" (Var "y")))

        cont1 :: Contex
        cont1 = [("true",Abs "x" (Abs "y" (Var "x")))
                ,("false",Abs "x" (Abs "y" (Var "y")))
                ,("test",Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n")))))
                ,("iszero",Abs "n" (App (App (Var "n") (Abs "x" (Var "false"))) (Var "true")))
                ,("plus",Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))))
                ,("mult",Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))))
                ,("pred",Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "x" (Var "x"))))))
                ,("fixM",Abs "f" (App (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y")))))))
                ,("sumR",Abs "r" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 0)) (App (App (Var "plus") (Var "n")) (App (Var "r") (App (Var "pred") (Var "n")))))))
                ,("factR",Abs "fact" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 1)) (App (App (Var "mult") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n")))))))
                ,("sum",App (Var "fixM") (Var "sumR"))
                ,("factor",App (Var "fixM") (Var "factR"))
                ]

        termS2 :: String
        termS2 = "(\\f g x. f x (g x))   (\\x y .x) (\\x y .x)"