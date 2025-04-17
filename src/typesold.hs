{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Types
-- Description : Declaration of classes and types for hero-wars utilities
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all types, data definition and classes
-- needed for the hero-wars utilities i am developing.
--
-- Original date: 5 Apr 2025
module Typesold
  ( Member (..),
    Pet (..),
    Titan (..),
    HasNameAndId (..),
  )
where

import GHC.Generics (Generic)
import Types.Assorted (ID, PHTName)

-- import Types.Guild (League (..))

type Lineup = (String, String, String, String, String)
-- ^ alias of a five string tuple to keeps the names of heros, pets, titans

-- ^ alias of Pet - Hero - Titan triple who keeps, shortname, name, id

-- | Structure to keep any member of a 'Guild' data
data Member = Member
  { -- | unique id
    mId :: ID,
    -- | the name of the hero
    mName :: String,
    -- | the 'Guild' he belongs. This is an existing 'ID' or Nothing
    guild :: Maybe ID,
    -- | max power of heros documented
    maxHPower :: Int,
    -- | max power of Titans documented
    maxTPower :: Int
  }
  deriving (Show, Generic)

-- |
-- Describe a polymorphic way to get id's, names and short names from various structures
class HasNameAndId a where
  getName :: a -> String
  -- ^ get the name

  getShortName :: a -> String
  -- ^ get the shortname. If no short name provided, it returns the 3 first characters of name

  getId :: a -> ID
  -- ^ get the unique id

instance HasNameAndId Member where
  getName = mName
  getShortName = take 3 . getName
  getId = mId

instance HasNameAndId Pet where
  getName (Pet (_, x, _)) = x
  getShortName (Pet (x, _, _)) = x
  getId (Pet (_, _, x)) = x

instance HasNameAndId Titan where
  getName (Titan (_, x, _)) = x
  getShortName (Titan (x, _, _)) = x
  getId (Titan (_, _, x)) = x

{- Pets  -}
data Pet = Pet {pDtls :: PHTName} deriving (Show)

data Titan = Titan {titanDtls :: PHTName} deriving (Show)

{-
 -- | Load a list from a json file (filepath) to a Maybe list of "a"
loadFromJson :: (FromJSON a) => FilePath -> IO (Maybe [a])
loadFromJson filepath = do
  contents <- B.readFile filepath
  return (decode contents)

-}
