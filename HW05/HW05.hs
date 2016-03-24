{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.List
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fp1 fp2 = do
  clue1 <- BS.readFile fp1
  clue2 <- BS.readFile fp2
  return $ BS.filter (/=0) $ BS.pack $ BS.zipWith xor clue1 clue2

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey s fp = do
  enc <- BS.readFile $ fp ++ ".enc"
  BS.writeFile fp $ BS.pack $ BS.zipWith xor (BS.cycle s) enc

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = do
  file <- BS.readFile fp
  return $ decode file

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs fp1 fp2 = do
  f1 <- decode <$> BS.readFile fp1
  f2 <- decode <$> BS.readFile fp2
  return $ filterer <$> f1 <*> f2
  where
    filterer :: Eq a => [a] -> [a] -> [a]
    filterer a = filter (`elem` a)

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = helper ts Map.empty
  where
    helper :: [Transaction] -> Map String Integer -> Map String Integer
    helper [] m = m
    helper (x:xs) m =
      let a = amount x in
      helper xs $ Map.insert (to x) a
                $ Map.insert (from x) (negate a) m

-- Exercise 6 -----------------------------------------
cmpByValue :: Ord a => (t, a) -> (t1, a) -> Ordering
cmpByValue (_,v1) (_,v2) = compare v1 v2

getCriminal :: Map String Integer -> String
getCriminal = fst . maximumBy cmpByValue . Map.toList

-- Exercise 7 -----------------------------------------
partitionWandL :: Map t Integer -> ([(t, Integer)], [(t, Integer)])
partitionWandL = partition ((>0) . snd) . sortBy cmpByValue . Map.toList

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = helper ws ls tids
  where
    (ws, ls) = partitionWandL m
    helper :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
    helper [] _ _ = []
    helper _ [] _ = []
    helper _ _ [] = []
    helper (w:ws) (l:ls) (i:ids)
      | snd w == 0 = helper ws (l:ls) (i:ids)
      | snd l == 0 = helper (w:ws) ls (i:ids)
      | otherwise  = let ammount = min (abs . snd $ w) (abs . snd $ l) in
        Transaction (fst w) (fst l) ammount i :
        helper ((fst w, snd w - ammount) : ws) ((fst l, snd l + ammount) : ls) ids

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
-- writeJSON = undefined
writeJSON fp = BS.writeFile fp . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

