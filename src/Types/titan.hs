-- |
-- Module      : Types.Titan
-- Description : All datastructures and functions about Heros!
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all datastructures for Titans and the function needed for
--
-- Original date: 17 Apr 2025
module Types.Titan (Titan (..)) where

import GHC.Generics (Generic)
import Types.Assorted (HasNameAndId (..), PHTName)

newtype Titan = Titan {titanDtls :: PHTName} deriving (Show)

instance HasNameAndId Titan where
  getName (Titan (_, x, _)) = x
  getShortName (Titan (x, _, _)) = x
  getId (Titan (_, _, x)) = x
