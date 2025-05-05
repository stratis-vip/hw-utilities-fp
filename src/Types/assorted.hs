{-# LANGUAGE DeriveGeneric #-}

module Types.Assorted
  ( IntDate (..),
    ID (..),
    Lineup,
    PHTName,
    HasNameAndId (..),
    toHeroLineUp,
    toPetLineUp,
    clearPwtLnup,
    Export (..)
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import GHC.Generics (Generic)

-- |
-- Describe a polymorphic way to get id's, names and short names from various structures
class HasNameAndId a where
  getName :: a -> String
  -- ^ get the name

  getShortName :: a -> String
  -- ^ get the shortname. If no short name provided, it returns the 3 first characters of name

  getId :: a -> ID
  -- ^ get the unique id

newtype IntDate = IntDate Int deriving (Show, Read, Generic, Eq)

newtype ID = ID Int deriving (Show, Read, Eq, Generic)

instance ToJSON ID where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ID

type Lineup = (String, String, String, String, String)
-- ^ alias of a five string tuple to keeps the names of heros, pets, titans

-- | return a Lineup object with 5 heros
toHeroLineUp :: [String] -> Lineup
toHeroLineUp [a, b, c, d, e] = (a, b, c, d, e)
toHeroLineUp _ = ("###", "###", "###", "###", "###")

-- | return a Lineup object with 5 pets
toPetLineUp :: [String] -> Lineup
toPetLineUp [a, b, c, d, e] = (a, b, c, d, e)
toPetLineUp _ = ("###", "###", "###", "###", "###")

-- | clear the "(xxxxx) part from the pets"
clearPwtLnup :: String -> [String]
clearPwtLnup x = map (takeWhile (/= '(')) $ words x

type PHTName = (String, String, ID)
-- ^ alias of Pet - Hero - Titan triple who keeps, shortname, name, id (P)et(H)ero(T)itan(Name)

instance ToJSON IntDate where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON IntDate


data Export a = Export { date :: Int , type_ :: String  , data_ :: [a]} deriving (Show, Generic)

instance ToJSON a => ToJSON (Export a) where
  toEncoding = genericToEncoding defaultOptions 
