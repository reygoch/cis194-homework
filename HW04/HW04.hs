{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = clean a == clean b

-- Exercise 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = if preppd /= [] then helper 0 preppd else "0"
        where
            preppd = clean p

            helper :: (Num a, Eq a, Show a) => Int -> [a] -> String
            helper _ [] = ""
            helper e ([c]) = format e c 
            helper e (0:cs) = helper (e + 1) cs
            helper e (c:cs) = helper (e + 1) cs ++ " + " ++ format e c 

            format :: (Num a, Eq a, Show a) => Int -> a -> String
            format _ 0 = "0"
            format 1 1 = "x"
            format 0 c = show c
            format 1 c = show c ++ "x" 
            format e 1 = "x^" ++ show e
            format e c = show c ++ "x^" ++ show e

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ helper a b
    where
        helper :: (Num a) => [a] -> [a] -> [a]
        helper aa [] = aa
        helper [] bb = bb
        helper (aa:as) (bb:bs) = aa + bb : helper as bs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = sum $ helper 0 a b
    where
        helper :: (Num a) => Int -> [a] -> [a] -> [Poly a] 
        helper _ [] _ = [0]
        helper c [aa] bs = [item c aa bs]
        helper c (aa:as) bs = item c aa bs : helper (c + 1) as bs
        item c aa bs = P $ replicate c 0 ++ map (*aa) bs

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P $ map (*(-1)) p 
    fromInteger n = P [fromInteger n] 
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P p) n = helper 0 n p 
    where
        helper :: (Num a) => Int -> a -> [a] -> a
        helper _ _ [] = 0
        helper e xx (c:cs) = c*xx^e + helper (e + 1) xx cs 

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 a = a 
    nderiv 1 a = deriv a 
    nderiv n a = nderiv (n - 1) $ deriv a 

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P []) = P [0]
    deriv (P (_:as)) = P $ helper 1 as
        where
            helper :: Num a => Int -> [a] -> [a]
            helper _ [] = [0]
            helper e [i] = [i * fromIntegral e]
            helper e (i:is) = (i * fromIntegral e) : helper (e + 1) is

-- Utilities ------------------------------------------
clean :: (Eq a, Num a) => [a] -> [a]
clean = reverse . dropWhile (==0) . reverse
