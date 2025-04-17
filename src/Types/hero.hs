{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Types.Hero
-- Description : All datastructures and functions about Heros!
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all datastructures for Heros and the function needed for
--
-- Original date: 17 Apr 2025
module Types.Hero (Hero (..)) where

import GHC.Generics (Generic)
import Types.Assorted (PHTName)

data Hero = Hero {heroDtls :: PHTName} deriving (Show)