{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Assorted
  ( IntDate (IntDate),
    ID (ID),
    Lineup,
    PHTName,
  )
where

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
import GHC.Generics (Generic)

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