{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Types.Guild
-- Description : All datastructures and functions arroung guilds!
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all datastructures for Guilds and the function needed for
--
-- Original date: 9 Apr 2025
module Types.Guild
  ( Guild (..),
  League (..),
  saveGuilds,
  readOthers,
  processGuildDataFiles 
  )
where

import Data.Aeson (FromJSON, ToJSON, decode, defaultOptions, encode, genericToEncoding, toEncoding)
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics (Generic)
import Helpers (readRawFile, removeQuotes, splitString)
import MyJson (saveToJson)
import Types.Assorted (ID(..), IntDate, Export (..))
import MyJson (saveObjectToJson )
import Text.Read (readMaybe )
class HeroWars a where
  getId :: a -> ID

instance HeroWars Guild where
  getId (Guild x _ _ _ _) = x

-- | League is the 4 types of leagues in the game
data League = Gold | Silver | Bronze | Qualifying deriving (Show, Generic)

-- | Guild is the data keeps the details of a guild
data Guild = Guild
  { gId :: ID, -- unique id
    gName :: String, -- name of guild may be empty string
    gPosition :: Maybe Int,
    gLeague :: Maybe League,
    gHeroes :: [(IntDate, ID)]
  }
  deriving (Show, Generic)

instance ToJSON Guild where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Guild

instance ToJSON League where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON League

-- | Return a list with all the Guild data provided as string list
updateGuild :: [[String]] -> League -> [Guild]
updateGuild [] _ = []
-- names are with quotes, so i removing them with removeQuotes
updateGuild ([posS, idS, name] : xs) lg = Guild id (removeQuotes name) pos (Just lg) [] : updateGuild xs lg
  where
    id = read ("ID " ++ idS) :: ID
    pos = readMaybe (posS) :: Maybe Int 

-- | silver and bronze data files are comma delimeted files with header
readOthers :: FilePath -> IO [Guild]
readOthers filepath = do
  rawInit <- readRawFile filepath ','
  let raw = drop 1 rawInit
  -- \^ first row has the headers and need to be dropped
  let guilds = updateGuild (map (take 3  ) raw) Silver
  return guilds

-- | gold data file is a tab delimeted file with no headers, so it have his own function
readGold :: FilePath -> IO [Guild]
readGold filepath = do
  raw <- readRawFile filepath '\t'
  let guilds = updateGuild (map (\x -> ["-"] ++ (take 2 x)) raw) Gold
  return guilds

-- | This function gets the path of gold silver and bronze data every week and update
-- the json files of guilds
processGuildDataFiles :: FilePath -> FilePath -> FilePath -> IO [Guild]
processGuildDataFiles gold silver bronze = do
  gdGuild <- readGold gold
  srGuild <- readOthers silver
  beGuild <- readOthers bronze
  return ( gdGuild ++ srGuild ++ beGuild)

saveGuilds :: IO ()
saveGuilds = do
  guilds <- processGuildDataFiles  "src/data/gold.txt" "src/data/silver.txt" "src/data/bronze.txt" 
  let l = Export 20250505 "Guild" guilds 
  saveObjectToJson l "./results/guilds.json"
  putStrLn "created file: results/guilds.json"

