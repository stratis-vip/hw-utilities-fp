{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Use isJust" #-}
{-# HLINT ignore "Hoist not" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- |
-- Module      : Types.Battle
-- Description : All datastructures and functions about Battles !
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all datastructures for Battles  and the function needed for
--
-- Original date: 9 Apr 2025
module Types.Battle
  ( HeroBattle (..),
    readRawHeroBtls,
    toHeroLineUp,
    toPetLineUp,
    toHeroBtlIO,
    HrChampionDtl (..),
    isBattleFair,
  )
where

import Data.Aeson (FromJSON, ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.Csv (FromNamedRecord (..), (.:))
import GHC.Conc (ensureIOManagerIsRunning)
import GHC.Generics (Generic)
import Helpers
  ( getTheListFromMaybeList,
    isNonEmpty,
    readRawFile,
    remove2ndElement,
    rmdups,
    safeReadInt,
    splitString,
  )
import Text.Read (readMaybe)
import Types.Assorted (ID (ID), IntDate (IntDate), Lineup, PHTName, clearPwtLnup, toHeroLineUp, toPetLineUp)
import Types.Consts (championsAcceptedPowerGap)

-- | Data keeps a battle with Heros.
data HeroBattle = HeroBattle
  { -- | date of battle in YYYMMDD form, as 'IntDate'
    hbDate :: IntDate,
    -- | attacker description as 'HrChampionDtl'
    hbAttacker :: HrChampionDtl,
    -- | defender description as 'HrChampionDtl'
    hbDefender :: HrChampionDtl,
    -- | point of the battle
    hbPoints :: Int
  }
  deriving (Show, Generic)

-- |
-- structure to keep all info about a champion of a 'Guild'
data HrChampionDtl = HrChampionDtl
  { -- | unique id ('ID') of an EXISTING member 'Member'
    member :: ID,
    name :: String,
    guildId :: ID,
    -- | power of this Member
    power :: Int,
    -- | +
    hLnup :: Lineup,
    petLnup :: Lineup,
    pet :: String
  }
  deriving (Show, Generic)

instance Eq HrChampionDtl where
  (==) (HrChampionDtl {power = p}) (HrChampionDtl {power = p1}) = p == p1
  (/=) (HrChampionDtl {power = p}) (HrChampionDtl {power = p1}) = p /= p1

instance Ord HrChampionDtl where
  compare (HrChampionDtl {power = p}) (HrChampionDtl {power = p1}) = compare p p1

instance ToJSON HeroBattle where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HeroBattle

instance ToJSON HrChampionDtl where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HrChampionDtl

instance FromNamedRecord HeroBattle where
  parseNamedRecord r = do
    dateVal <- r .: "date 8"
    points <- r .: "points"

    attIDVal <- r .: "attacker id"
    attName <- r .: "att name"
    attGuildId <- r .: "att team id"
    attPowerVal <- r .: "att power"
    attTeamVal <- r .: "att team"
    attTeamPetVal <- r .: "attacker patronage"

    defIDVal <- r .: "defender id"
    defName <- r .: "def name"
    defGuildId <- r .: "def team id"
    defPowerVal <- r .: "def power"
    defTeamVal <- r .: "team"
    defTeamPetVal <- r .: "defender patronage"

    let date = IntDate dateVal
    let attTeamList = drop 1 $ words attTeamVal
    let attPetList = map (takeWhile (/= '(')) $ words attTeamPetVal
    let attPet = take 3 attTeamVal
    let defTeamList = init $ words defTeamVal
    let defPet = last $ words defTeamVal
    let defPetList = map (takeWhile (/= '(')) $ words defTeamPetVal
    let attacker =
          HrChampionDtl
            (ID attIDVal)
            attName
            (ID attGuildId)
            attPowerVal
            (toHeroLineUp attTeamList)
            (toPetLineUp attPetList)
            attPet
    let defender =
          HrChampionDtl
            (ID defIDVal)
            defName
            (ID defGuildId)
            defPowerVal
            (toHeroLineUp defTeamList)
            (toPetLineUp defPetList)
            defPet

    return $
      HeroBattle date attacker defender points

{-readHeroBtls :: FilePath -> IO [HeroBattle]
readHeroBtls filepath = do
  raw <- readRawFile filepath ','
-}
readRawHeroBtls :: IO ()
readRawHeroBtls = do
  contents <- readFile "src/data/athHerosTwo.csv"
  let nonEmptyLines = lines contents

  let validRecords = filter checkHeroBattle $ map (splitString ',') nonEmptyLines

  -- let createHeroBtlsObjects = map (toHeroBtlIO) validRecords
  let final = rmdups $ map (getTheListFromMaybeList . remove2ndElement) validRecords

  print (map toHeroBtlIO final)

toHeroBtl :: [String] -> Maybe HeroBattle
toHeroBtl [date, aId, aName, aPower, aLnup, dId, dName, dPower, dLnup, points, aPetLnup, dPetLnup, aGuild, bGuild] =
  Just (HeroBattle bDate bAtt bDef bPoints)
  where
    bDate = read ("IntDate " ++ date) :: IntDate
    bAtt = HrChampionDtl (read ("ID " ++ aId) :: ID) aName (read ("ID " ++ aGuild) :: ID) (read aPower :: Int) (toHeroLineUp (words aLnup)) (toPetLineUp $ clearPwtLnup aPetLnup) (head (words aLnup))
    bDef = bAtt
    bPoints = read points :: Int
toHeroBtl _ = Nothing

toHeroBtlIO :: [String] -> HeroBattle
toHeroBtlIO [date, aId, aName, aPower, aLnup, dId, dName, dPower, dLnup, points, aPetLnup, dPetLnup, aGuild, bGuild] =
  HeroBattle bDate bAtt bDef bPoints
  where
    -- bAtt bDef bPoints)

    bDate = read ("IntDate " ++ date) :: IntDate
    bAtt = HrChampionDtl (read ("ID " ++ aId) :: ID) aName (read ("ID " ++ aGuild) :: ID) (read aPower :: Int) (toHeroLineUp (words aLnup)) (toPetLineUp $ clearPwtLnup aPetLnup) (head (words aLnup))
    bDef = HrChampionDtl (read ("ID " ++ dId) :: ID) dName (read ("ID " ++ bGuild) :: ID) (read dPower :: Int) (toHeroLineUp (words dLnup)) (toPetLineUp $ clearPwtLnup dPetLnup) (head (words dLnup))

    bPoints = read points :: Int

-- Define expected structure for HeroBattle/
checkHeroBattle :: [String] -> Bool
checkHeroBattle xs
  | length xs /= 15 = False -- Must match your array length
  | otherwise =
      let [date, _, aId, aName, aPower, aLnup, dId, dName, dPower, dLnup, points, aPetLnup, dPetLnup, aGuild, bGuild] = xs
       in maybe False (const True) (safeReadInt date)
            && checkHeroDetail [aId, aPower, aName, aLnup, aGuild] -- bDate :: IntDate
            && checkHeroDetail [dId, dPower, dName, dLnup, bGuild] -- bAttacker
            && maybe False (const True) (safeReadInt points) -- bDefender
            && isValidLineup 5 aPetLnup -- bPoints :: Int
            && isValidLineup 5 dPetLnup -- petLnup for attacker

-- | Check if a string is a valid Lineup (5 space-separated items)
isValidLineup :: Int -> String -> Bool
isValidLineup n s = length (words s) == n && all (not . null) (words s)

-- | Define expected structure for HeroDetail
checkHeroDetail :: [String] -> Bool
checkHeroDetail [heroId, power, name, lineup, guild] =
  maybe False (const True) (safeReadInt heroId)
    && maybe False (const True) (safeReadInt power) -- hero :: HeroID (Int)
    && maybe False (const True) (safeReadInt guild) -- Guild Id
    && isNonEmpty name
    && isValidLineup 6 lineup -- pet :: String
    -- hLnup :: Lineup
checkHeroDetail _ = False

class HasChampions a where
  getAttPower :: a -> Int
  getDefPower :: a -> Int

instance HasChampions HeroBattle where
  getAttPower :: HeroBattle -> Int
  getAttPower x = power (hbAttacker x)
  getDefPower :: HeroBattle -> Int
  getDefPower x = power (hbDefender x)

isBattleFair :: (HasChampions a) => a -> Bool
isBattleFair x = def >= att || abs (att - def) <= championsAcceptedPowerGap
  where
    att = getAttPower x
    def = getDefPower x