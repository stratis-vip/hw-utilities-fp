-- {-# LANGUAGE DeriveGeneric #-}

{- |
Module      : MyJson
Description : All functions about json handling
Copyright   : (c) Stratis Christodoulou 2025
Maintainer  : stratis.vip@gmail.com
Stability   : experimental

This module holds all functions about json handling

Original date: 9 Apr 2025
-}
module MyJson (saveToJson, loadFromJson, saveObjectToJson ) where

import Data.Aeson (FromJSON, ToJSON, decode, defaultOptions, encode, genericToEncoding, toEncoding)
import qualified Data.ByteString.Lazy.Char8 as B

-- import Data.Text (Text)
-- import qualified Data.Text.IO as T
-- import GHC.Generics (Generic)
{-import Typesold (Member (..))

instance ToJSON Member where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Member

loadMembersFromJson :: FilePath -> IO (Maybe [Member])
loadMembersFromJson filepath = do
  contents <- B.readFile filepath
  return (decode contents)
-}

{- | Load a list from a json file (filepath) to a Maybe list of "a"
 example of use:
 loadFromJson "guilds.json" :: IO (Maybe [Guild])
-}

loadFromJson :: (FromJSON a) => FilePath -> IO (Maybe [a])
loadFromJson filepath = do
  contents <- B.readFile filepath
  return (decode contents)

-- | Save the list to filepath
saveToJson :: (ToJSON a) => [a] -> FilePath -> IO ()
saveToJson xs filepath = B.writeFile filepath (encode xs)

saveObjectToJson :: (ToJSON a) => a -> FilePath -> IO()
saveObjectToJson x fp = B.writeFile fp (encode x)
