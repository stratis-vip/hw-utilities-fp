{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isJust" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use print" #-}
{-# HLINT ignore "Hoist not" #-}

module MyLib (main3, Export (..)) where

import Helpers
  ( getCurrentTimeToIntDate,
    getTheListFromMaybeList,
    remove2ndElement,
    rmdups,
    splitString
  )
import HwData.Values (allHeros, allPets, allTitans)
import System.IO (Handle, IOMode (ReadMode), hGetLine, hIsEOF, hSetEncoding, stdout, utf8, withFile)
import Text.Read (readMaybe)
import Types.Assorted (HasNameAndId (getName), ID (ID), IntDate (IntDate), Export (..) )
import Types.Battle
  ( HeroBattle (..),
    HrChampionDtl (..),
    isBattleFair,
    readRawHeroBtls,
    toHeroBtlIO,
    toHeroLineUp,
    toPetLineUp,
  )
import Types.Guild (Guild (Guild))
import Types.Pet (isLineupValid, lineupLookup, lineupToList)
import MyJson (saveObjectToJson )

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
        -- TODO i will spread the line to a HeroBattle record
        Nothing -> putStrLn "ERROR"
      processLines handle

main :: IO ()
main = withFile "src/athHerosTwo.csv" ReadMode processLines

-- | safe Parse Int
safeReadInt :: String -> Maybe Int
safeReadInt = readMaybe

isNonEmpty :: [a] -> Bool
isNonEmpty = not . null

-- | Check if a string is a valid Lineup (n space-separated items)
isStringAValidLineup :: Int -> String -> Bool
isStringAValidLineup n s = length (words s) == n && all (not . null) (words s) -- HLINT ignore

-- Define expected structure for HeroDetail
checkHeroDetail :: [String] -> Bool
checkHeroDetail [heroId, power, name, lineup, guild] =
  maybe False (const True) (safeReadInt heroId)
    && maybe False (const True) (safeReadInt power) -- hero :: HeroID (Int)
    && maybe False (const True) (safeReadInt guild) -- Guild Id
    && isNonEmpty name
    && isStringAValidLineup 6 lineup -- pet :: String
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
            && isStringAValidLineup 5 aPetLnup -- bPoints :: Int
            && isStringAValidLineup 5 dPetLnup -- petLnup for attacker

main3 :: IO ()
main3 = do
  hSetEncoding stdout utf8
  contents <- readFile "src/athHerosAllClean.csv"
  let nonEmptyLines = lines contents

  let validRecords = filter checkHeroBattle $ map (splitString ',') nonEmptyLines

  let final = rmdups $ map (getTheListFromMaybeList . remove2ndElement) validRecords

  print final

-- prepareStm :: [a] -> String
prepareStm x = concat (take 1 semiFinal ++ drop 2 semiFinal)
  where
    semiFinal = drop 4 $ reverse . drop 2 . reverse $ x

r = ["20250405", "4868542", "stratis", "1104330", "Kho Dor Aug Ori Neb Dan", "4704912", "Linni", "1058756", "Ruf Ish Jhu Faf Mar Kho", "20", "Mer(11064) Axe(11064) Bis(11064)  ###  Cai(11064)", "Bis(11064) Fen(11064) Vex(10154) Oli(11064) Axe(11064)", "183303", "39706"]

s = ["20250405", "4454896", "Team D", "1154313", "Axe Aug Ori Seb Isa Aur", "6750054", "nobody", "1004039", "Dan Neb Ori Aug Dor Kho", "20", "Kho(11064) Axe(11064) Fen(7804) Oli(11064) Cai(11064)", "Cai(11064)  ###  Axe(11064) Alb(11064) Mer(11064)", "183303", "39706"]

rec = ["20250405", "4868542", "stratis", "1104330", "Kho Dor Aug Ori Neb Dan", "4704912", "Linni", "1058756", "Ruf Ish Jhu Faf Mar Kho", "20", "Mer(11064) Axe(11064) Bis(11064)  ###  Cai(11064)", "Bis(11064) Fen(11064) Vex(10154) Oli(11064) Axe(11064)", "183303", "39706"]

c2 = HrChampionDtl (ID 35) "poppy" (ID 3) 2000 ("a1", "b1", "c1", "d1", "e1") ("p1", "q1", "r1", "s1", "t1") "kho"

c1 = HrChampionDtl (ID 34) "stratis" (ID 2) 1000 ("a", "b", "c", "d", "e") ("p", "q", "r", "s", "t") "axe"

b = HeroBattle {hbDate = IntDate 20250412, hbAttacker = HrChampionDtl {member = ID 4712527, name = "HRCAK", guildId = ID 183303, power = 1140090, hLnup = ("Mar", "Seb", "Mor", "Yas", "Aur"), petLnup = ("Oli", "Alb", "Axe", "Fen", "Cai"), pet = "Oli"}, hbDefender = HrChampionDtl {member = ID 4341747, name = "erdosman", guildId = ID 39706, power = 1086979, hLnup = ("Ruf", "K'A", "Dan", "Seb", "Mar"), petLnup = ("Oli", "Fen", "Cai", "Alb", "Axe"), pet = "Axe"}, hbPoints = 20}

