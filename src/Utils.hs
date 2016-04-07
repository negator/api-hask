{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Lib
     where

import           Data.Array
import           Data.Char
import           Data.List
import           Data.String
import           Data.String.Utils

-- Levensthein distance between a pair of strings
-- Optimized for performance, using a memoized array
levenshtein::String -> String -> Int
levenshtein s1 s2 = dist lfst lsnd
    where
      (fst, snd) = (fmap toLower s1 , fmap toLower s2)
      (lfst, lsnd) = (length fst, length snd)
      table = array ((0,0),(lfst, lsnd)) [((i,j), dist i j) | i <- [0..lfst], j <- [0..lsnd]]

      dist :: Int -> Int -> Int
      dist i 0 = i
      dist 0 j = j
      dist i j
          | fst !! (i-1) == snd !! (j-1) = table ! (i - 1, j - 1)
          | otherwise = 1 + minimum [table ! (i - 1, j) , table ! (i,j - 1), table ! (i - 1,j - 1)]


{-
Fuzzy match algorithm:
	1. Tokenize and sort each input string.
	2. Compute left, right set difference and set intersection
	3. Create strings from each set (space separated)
	3. Return maximum score among each pair. Where score is edit distance divided by max length of either input
-}

fuzzymatch :: String -> String -> Double
fuzzymatch "" _ = 0.0
fuzzymatch _ "" = 0.0
fuzzymatch s1 s2 = maximum  (uncurry(score) `fmap` pairs(ilr))
  where
    [left, right] = (sort . words . filter ((||) <$> isLetter <*> isSpace)) `fmap` [s1, s2]
    itsct = left `intersect` right
    ilr = unwords `fmap` [itsct, itsct ++ (left \\ itsct), itsct ++ (right \\ itsct)]

    score :: String -> String -> Double
    score l r = 1 - (fromIntegral(levenshtein l r) / (maximum ((fromIntegral . length) `fmap`  [l, r])))


pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (h:t) = ((h,) `fmap` t) ++ (pairs t)

getOrElse :: Maybe a -> a -> a
getOrElse (Just a) _ = a
getOrElse _ a = a

