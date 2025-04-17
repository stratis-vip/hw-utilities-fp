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
  ( 
    Titan (..),
    HasNameAndId (..),
  )
where

import GHC.Generics (Generic)
import Types.Assorted (HasNameAndId (..), ID, PHTName)

-- import Types.Guild (League (..))

-- ^ alias of Pet - Hero - Titan triple who keeps, shortname, name, id


instance HasNameAndId Titan where
  getName (Titan (_, x, _)) = x
  getShortName (Titan (x, _, _)) = x
  getId (Titan (_, _, x)) = x



data Titan = Titan {titanDtls :: PHTName} deriving (Show)

{-
 -- | Load a list from a json file (filepath) to a Maybe list of "a"
loadFromJson :: (FromJSON a) => FilePath -> IO (Maybe [a])
loadFromJson filepath = do
  contents <- B.readFile filepath
  return (decode contents)

-}
