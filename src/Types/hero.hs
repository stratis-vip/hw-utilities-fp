{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

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
import Types.Assorted (HasNameAndId (..), ID (ID), PHTName)

newtype Hero = Hero {heroDtls :: PHTName} deriving (Show, Eq)

instance HasNameAndId Hero where
  getName :: Hero -> String
  getName (Hero (_, x, _)) = x
  getShortName :: Hero -> String
  getShortName (Hero (x, _, _)) = x
  getId :: Hero -> ID
  getId (Hero (_, _, x)) = x