{-# OPTIONS_GHC -Wall #-}
module Bohuta06 where

    newtype Poly a = P [a]

    x :: Num a => Poly a
    x = P [0,1]

    instance (Num a, Eq a) => Eq (Poly a) where
        P[] == P[] = True
        P[] == P z | all (==0) z = True
        P(f:l) == P(f1:l1) = f == f1 && l == l1
        _ == _ = False

    instance (Num a, Eq a, Show a) => Show (Poly a) where
        show(P[]) = show(0 :: Integer)
        show(P[a]) = show a
        show(P l)
            | all (==0) l = show(0 :: Integer)
            | otherwise = last ([helpShow (l!!i) i | i <- [0..(length l-1)], l!!i /= 0]) ++
                   concatMap (" + "++) (tail (reverse[helpShow (l!!i) i | i <- [0..(length l-1)], l!!i /= 0]))
                   where helpShow char pow
                            | pow == 0 = show char
                            | char == 1 && pow == 1 = "x"
                            | char == -1 && pow == 1 = "-x"
                            | pow == 1 = show char ++ "x"
                            | char == 1 = "x^" ++ show pow
                            | char == -1 = "-x^" ++ show pow
                            | otherwise = show char ++ "x^" ++ show pow



    plus :: Num a => Poly a -> Poly a -> Poly a
    plus (P[]) (P[]) = P[]
    plus (P p1) (P p2)
            | length p1 < length p2 = P([p1!!i + p2!!i| i <- [0..length p1-1]] ++ [p2!!i|i<-[length p1 .. length p2-1]])
            | length p2 < length p1 = P([p1!!i + p2!!i| i <- [0..length p2-1]] ++ [p1!!i|i<-[length p2 .. length p1-1]])
            | otherwise = P([p1!!i + p2!!i| i <- [0..length p2-1]])

    times :: Num a => Poly a -> Poly a -> Poly a
    times (P[]) (P[]) = P[]
    times (P[]) p = p
    times p (P[]) = p
    times (P l1) p = foldl1 plus [multiply i (l1!!i) p| i <- [0..length l1 -1]]

    instance Num a => Num (Poly a) where
        (+) = plus
        (*) = times
        negate (P l) = P [-i|i <- l]
        fromInteger i = P [fromInteger i]
        abs    = undefined
        signum = undefined

    applyP :: Num a => Poly a -> a -> a
    applyP (P p) e = sum [(p!!i) * e^i| i<-[0..length p-1]]

    class Num a => Differentiable a where
        derive  :: a -> a
        nderive :: Int -> a -> a
        nderive 1 p = derive p
        nderive a p = derive (nderive (a-1) p)

    instance Num a => Differentiable (Poly a) where
        derive (P p) = P [ (p!!i) * fromIntegral i | i<- [1..length p - 1]]


    multiply :: Num a => Int -> a -> Poly a -> Poly a
    multiply n elem1 (P p)
            | n == 0 = P [elem1 * i|i<-p]
            | otherwise = P ([0| _ <- [0..n-1]] ++ [elem1 * i|i<-p])

    