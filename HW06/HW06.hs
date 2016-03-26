{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

-- I'll do a 0 based version :D
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 0 : 1 : (zipWith (+) fibs2 $ tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
  show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
    ++ ",..."

  streamToList :: Stream a -> [a]
  streamToList (Cons val stream) = val : (streamToList stream)

-- Exercise 4 -----------------------------------------

instance Functor Stream where
  fmap f (Cons val stream) = f val `Cons` (fmap f stream)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = x `Cons` (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate r s = s `Cons` helper r s
  where
    helper f x = f x `Cons` (helper f $ f x)

    sInterleave :: Stream a -> Stream a -> Stream a
    sInterleave (Cons v s1) s2 = v `Cons` (sInterleave s2 s1)

    sTake :: Int -> Stream a -> [a]
    sTake 0 _ = []
    sTake n (Cons v s) = v : sTake (n - 1) s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sRepeat 0 `sInterleave` (sRepeat 1 `sInterleave` (sRepeat 2 `sInterleave` (sRepeat 3 `sInterleave` (sRepeat 4))))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate (\x -> 1103515245 * x + 12345 `mod` 2147483648) 

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ~13 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ~6 KB -} -- this shit is raaaad!
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just $ helper xs x x
  where
    helper [] mn mx = (mn, mx)
    helper (x:xs) !mn !mx = helper xs (min x mn) (max x mx)

    main :: IO ()
    main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------
data Matrix n = Mx {a :: n, b :: n, c :: n,  d :: n} deriving (Show)

instance (Num n) => Num (Matrix n) where
	(*) (Mx a b c d) (Mx e f g h) = Mx (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib n = a $ (Mx 1 1 1 0) ^ n