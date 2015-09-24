{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = clean a == clean b
        where
            clean :: (Num a, Eq a) => [a] -> [a]
            clean = reverse . dropWhile (==0) . reverse

-- Exercise 3 -----------------------------------------
-- x^3 + 2x^2 + 1
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = creator 0 p
        where
            creator :: (Num a, Eq a, Show a) => Int -> [a] -> String
            creator _ [] = ""
            creator e [c] = part e c
            creator e (0:cs) = creator (e+1) cs
            creator e (c:cs) = creator (e+1) cs ++ " + " ++ part e c

            part :: (Num a, Eq a, Show a) => Int -> a -> String
            part 0 c = show c
            part e 1 = "x^" ++ show e
            part e (-1) = "-x^" ++ show e
            part 1 c = show c ++ "x"
            part e c = show c ++ "x^" ++ show e

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ sumup a b
    where
        sumup :: Num a => [a] -> [a] -> [a]
        sumup [] [] = []
        sumup [] lb = lb
        sumup la [] = la
        sumup (la:las) (lb:lbs) = la + lb : sumup las lbs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined