{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mv = mv >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 v = liftM2
  (\v1 v2 -> v // [(i1, v2), (i2, v1)]) (v V.!? i1) (v V.!? i2)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v!?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = getRandomR (0, V.length v - 1) >>= return . (v !?)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.fromList <$!> replicateM n getRandom

-- just for the sake of variety :D
randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR 0 _ = return V.empty
randomVecR n b = liftM2 cons (getRandomR b) (randomVec (n - 1))

-- Exercise 5 -----------------------------------------
swap :: (Int, Int) -> Vector a -> Vector a
swap (i, j) v = v // [(i, v ! j), (j, v ! i)]

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = forM [n-1, n-2..1] genSwap >>= return . foldr swap v
  where
    n = V.length v
    genSwap i = (\j -> (i, j)) <$> getRandomR (0, i)

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (below, p, above)
  where
    p = v ! i
    (below, above) = V.unstablePartition (< p) $ V.tail $ swap (0, i) v

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | V.null v = V.empty
  | otherwise = qsort below <> (p `cons` qsort above)
  where
    (below, p, above) = partitionAt v 0

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | V.null v = return V.empty
  | otherwise = parts >>= mergeSort
  where
    parts = getRandomR (0, V.length v - 1) >>= return . partitionAt v
    mergeSort (b, p, t) = liftM2 (<>) (qsortR b) (liftM (cons p) (qsortR t))


-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  | V.null v = return Nothing
  | otherwise = liftM (partitionAt v) rpi >>= helper
  where
    rpi = getRandomR (0, V.length v - 1)
    helper (bottom, pivot, top)
      | i < blen = select i bottom
      | i > blen = select (i - blen - 1) top
      | otherwise = return $ Just pivot
      where blen = V.length bottom

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card label suit | suit <- suits, label <- labels]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | V.null d = Nothing
  | otherwise = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n = helper 0 []
  where
    helper :: Int -> [Card] -> Deck -> Maybe ([Card], Deck)
    helper 0 cs dick = return (cs, dick)
    helper i cs dick = do
      (c, d) <- nextCard dick
      helper (n + 1) (c:cs) d

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100