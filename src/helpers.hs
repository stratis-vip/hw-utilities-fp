{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
-- |
-- Module      : Helpers
-- Description : Every single utility finction to help the program
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all simple functions needed to help us
--
-- Original date: 6 Apr 2025
module Helpers
  ( IntDate,
    fromIntDate,
    splitString,
    remove2ndElement,
    removeQuotes,
    readRawFile,
    trim,
    getCurrentTimeToIntDate,
    safeReadInt,
    isNonEmpty,
    rmdups,
    getTheListFromMaybeList,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Read
import Types.Assorted (IntDate)


-- | Finds the indexed value from an array [(a,b)] where the index is a
idx :: (Eq a) => a -> [(a, b)] -> Maybe b
idx = lookup

-- | String Index of months
allMonths :: [(String, String)]
allMonths =
  [ ("01", "Jan"),
    ("02", "Feb"),
    ("03", "Mar"),
    ("04", "Apr"),
    ("05", "May"),
    ("06", "Jun"),
    ("07", "Jul"),
    ("08", "Aug"),
    ("09", "Sep"),
    ("10", "Oct"),
    ("11", "Nov"),
    ("12", "Dec")
  ]

-- | Returns the 3letter month out of month part of IntDate YYYYMMDD
getMonth :: String -> [(String, String)] -> String
getMonth m = fromMaybe "???" . lookup m

-- | Returns a string with the date in the form DD-MMM-YYYY from the IntDate YYYYMMDD
fromIntDate :: Int -> String
fromIntDate x = day ++ "-" ++ month ++ "-" ++ year
  where
    dString = show x
    (year, rest1) = splitAt 4 dString
    (monthCode, rest2) = splitAt 2 rest1
    day = take 2 rest2
    month = getMonth monthCode allMonths

-- | Splits a string on a given character and trims whitespace around each resulting substring
splitString :: Char -> String -> [String]
splitString delim = map trim . foldr step [""]
  where
    step x acc@(a : as)
      | x == delim = "" : acc
      | otherwise = (x : a) : as

-- | Returns a string with no spaces in front or after the given string
trim :: String -> String
trim = T.unpack . T.strip . T.pack

-- | Removes the 2nd element of a List (if there is one) and return Just List or Nothing
remove2ndElement :: (Eq a) => [a] -> Maybe [a]
remove2ndElement (x : _ : xs) = Just (x : xs)
remove2ndElement _ = Nothing

-- | Removes all quotes from the start and the end of the string
removeQuotes :: String -> String
removeQuotes = dropWhile (== '"') . reverse . dropWhile (== '"') . reverse

-- | Opens file "filepath" and return an List of all guilds data. file is delimeted with ch character
readRawFile :: FilePath -> Char -> IO [[String]]
readRawFile filepath ch = do
  contents <- readFile filepath
  return (map (splitString ch) $ filter (/= []) $ lines contents)

-- | Returnw the current IntDate from the current Time
getCurrentTimeToIntDate :: IO Int
getCurrentTimeToIntDate = do
  currentTime <- getCurrentTime
  let currentDate = utctDay currentTime -- Extract only the date
  let formattedDate = formatTime defaultTimeLocale "%Y%m%d" currentDate
  return (read formattedDate :: Int)

-- | Removes duplicates of a List
rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

-- | Returns the Just Int from read x :: Int
safeReadInt :: String -> Maybe Int
safeReadInt = readMaybe

-- | Checks if a list is nont Empty
isNonEmpty :: [a] -> Bool
isNonEmpty = not . null

getTheListFromMaybeList :: Maybe [a] -> [a]
getTheListFromMaybeList x = case x of -- HLINT ignore
  Nothing -> []
  Just y -> y