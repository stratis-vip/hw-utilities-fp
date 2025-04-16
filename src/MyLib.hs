-- Enable the DeriveGeneric extension
-- {-# LANGUAGE DeriveGeneric #-}

-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isJust" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use print" #-}

module MyLib (main3) where

import Helpers (getCurrentTimeToIntDate, remove2ndElement, splitString)
import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, hSetEncoding, stdout, utf8, withFile)
import Text.Read (readMaybe)
import Types.Battle (HeroBattle (..), readRawHeroBtls, toHeroBtlIO, toHeroLineUp, toPetLineUp)
import Types.Guild (Guild (Guild))

-- Read and process lines recursively
processLines :: Handle -> IO ()
processLines handle = do
  eof <- hIsEOF handle
  if eof
    then return ()
    else do
      line <- hGetLine handle
      -- \^ Read a line from the file
      let array = splitString ',' line
      -- \^ Split the line in a array
      case remove2ndElement array of
        -- \^ remove the 2nd element, which is the date with hour
        Just xs -> putStrLn $ show xs -- HLINT ignore
        -- ^ TODO i will spread the line to a HeroBattle record
        Nothing -> putStrLn "ERROR"
      processLines handle

main :: IO ()
main = withFile "src/athHerosTwo.csv" ReadMode processLines

-- | safe Parse Int
safeReadInt :: String -> Maybe Int
safeReadInt = readMaybe

isNonEmpty :: [a] -> Bool
isNonEmpty = not . null

-- | Check if a string is a valid Lineup (5 space-separated items)
isValidLineup :: Int -> String -> Bool
isValidLineup n s = length (words s) == n && all (not . null) (words s) -- HLINT ignore

-- Define expected structure for HeroDetail
checkHeroDetail :: [String] -> Bool
checkHeroDetail [heroId, power, name, lineup, guild] =
  maybe False (const True) (safeReadInt heroId)
    && maybe False (const True) (safeReadInt power) -- hero :: HeroID (Int)
    && maybe False (const True) (safeReadInt guild) -- Guild Id
    && isNonEmpty name
    && isValidLineup 6 lineup -- pet :: String
    -- hLnup :: Lineup
checkHeroDetail _ = False

-- Define expected structure for HeroBattle
checkHeroBattle :: [String] -> Bool
checkHeroBattle xs
  | length xs /= 15 = False -- Must match your array length
  | otherwise =
      let [date, _, aId, aName, aPower, aLnup, dId, dName, dPower, dLnup, points, aPetLnup, dPetLnup, aGuild, bGuild] = xs
       in maybe False (const True) (safeReadInt date)
            && checkHeroDetail [aId, aPower, aName, aLnup, aGuild] -- bDate :: IntDate
            && checkHeroDetail [dId, dPower, dName, dLnup, bGuild] -- bAttacker
            && maybe False (const True) (safeReadInt points) -- HLINT ignore
            -- \^ bDefender
            && isValidLineup 5 aPetLnup -- bPoints :: Int
            && isValidLineup 5 dPetLnup -- petLnup for attacker

main3 :: IO ()
main3 = do
  hSetEncoding stdout utf8
  contents <- readFile "src/athHerosAllClean.csv"
  let nonEmptyLines = lines contents

  let validRecords = filter checkHeroBattle $ map (splitString ',') nonEmptyLines

  let final = rmdups $ map (retMaybeArray . remove2ndElement) validRecords

  print final

retMaybeArray :: Maybe [a] -> [a]
retMaybeArray x = case x of
  Nothing -> []
  Just y -> y

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) (rmdups xs)

-- prepareStm :: [a] -> String
prepareStm x = concat (take 1 semiFinal ++ drop 2 semiFinal)
  where
    semiFinal = drop 4 $ reverse . drop 2 . reverse $ x

r = ["20250405", "4868542", "stratis", "1104330", "Kho Dor Aug Ori Neb Dan", "4704912", "Linni", "1058756", "Ruf Ish Jhu Faf Mar Kho", "20", "Mer(11064) Axe(11064) Bis(11064)  ###  Cai(11064)", "Bis(11064) Fen(11064) Vex(10154) Oli(11064) Axe(11064)", "183303", "39706"]

s = ["20250405", "4454896", "Team D", "1154313", "Axe Aug Ori Seb Isa Aur", "6750054", "nobody", "1004039", "Dan Neb Ori Aug Dor Kho", "20", "Kho(11064) Axe(11064) Fen(7804) Oli(11064) Cai(11064)", "Cai(11064)  ###  Axe(11064) Alb(11064) Mer(11064)", "183303", "39706"]

rec = ["20250405", "4868542", "stratis", "1104330", "Kho Dor Aug Ori Neb Dan", "4704912", "Linni", "1058756", "Ruf Ish Jhu Faf Mar Kho", "20", "Mer(11064) Axe(11064) Bis(11064)  ###  Cai(11064)", "Bis(11064) Fen(11064) Vex(10154) Oli(11064) Axe(11064)", "183303", "39706"]
