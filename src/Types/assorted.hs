{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Assorted
  ( IntDate (IntDate),
    ID (ID),
    Lineup,
    PHTName,
    HasNameAndId (..),
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

type PHTName = (String, String, ID)
-- ^ alias of Pet - Hero - Titan triple who keeps, shortname, name, id (P)et(H)ero(T)itan(Name)

instance ToJSON IntDate where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON IntDate