{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Types.Member
-- Description : All datastructures and functions about Guild's members!
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all datastructures for members and the function needed for
--
-- Original date: 17 Apr 2025
module Types.Member (Member (..)) where

import GHC.Generics (Generic)
import Types.Assorted (HasNameAndId (..), ID)

import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)
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

instance FromJSON Member
instance ToJSON Member where 
  toEncoding = genericToEncoding defaultOptions

instance HasNameAndId Member where
  getName = mName
  getShortName = take 3 . getName
  getId = mId
