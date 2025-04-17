-- |
-- Module      : Types.Pet
-- Description : All datastructures and functions about Heros!
-- Copyright   : (c) Stratis Christodoulou 2025
-- Maintainer  : stratis.vip@gmail.com
-- Stability   : experimental
--
-- This module holds all datastructures for pets and the function needed for
--
-- Original date: 17 Apr 2025
module Types.Pet (Pet (..)) where

import GHC.Generics (Generic)
import Types.Assorted (HasNameAndId (..), PHTName)

{- Pets  -}
data Pet = Pet {pDtls :: PHTName} deriving (Show)

instance HasNameAndId Pet where
  getName (Pet (_, x, _)) = x
  getShortName (Pet (x, _, _)) = x
  getId (Pet (_, _, x)) = x