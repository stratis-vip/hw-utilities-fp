{-# LANGUAGE InstanceSigs #-}

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
module Types.Pet (Pet (..), lineupLookup, lineupToList, isLineupValid) where

import Data.Char (toLower)
import GHC.Generics (Generic)
import Types.Assorted (HasNameAndId (..), ID (ID), Lineup, PHTName)

{- Pets  -}
newtype Pet = Pet {pDtls :: PHTName} deriving (Show, Eq)

instance HasNameAndId Pet where
  getName :: Pet -> String
  getName (Pet (_, x, _)) = x
  getShortName :: Pet -> String
  getShortName (Pet (x, _, _)) = x
  getId :: Pet -> ID
  getId (Pet (_, _, x)) = x

lineupLookup :: (HasNameAndId a) => String -> [a] -> Maybe a
lineupLookup _ [] = Nothing
lineupLookup s (x : xs)
  | lowX == lowS = Just x
  | otherwise = lineupLookup s xs
  where
    lowX = map toLower $ getShortName x
    lowS = map toLower s

lineupToList :: Lineup -> [String]
lineupToList (a, b, c, d, e) = [a, b, c, d, e]

isLineupValid :: (Eq a, HasNameAndId a) => Lineup -> [a] -> Bool
isLineupValid _ [] = False
isLineupValid ln list = allLengthAre3 && allNameExists
  where
    allLengthAre3 = all ((== 3) . length) (lineupToList ln)
    allNameExists = all (\x -> lineupLookup x list /= Nothing) $ lineupToList ln
