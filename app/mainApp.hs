module Main ( main)where

import Control.Monad (mapM_)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (decodeByName)
import Data.Csv.Incremental (Parser (Done))
import Data.Int (Int)
import qualified Data.Vector as V
import MyLib (main3)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import Types.Battle (HeroBattle (HeroBattle))

main :: IO ()
main = do
  choice <- getInputChoice
  case choice of
    0 -> exitSuccess
    _ -> do
      print choice
      csvData <- BL.readFile "src/data/athHerosAll.csv"
      case decodeByName csvData of
        Left err -> putStrLn $ "Error parsing CSV: " ++ err
        Right (header, records) -> do
          putStrLn "Header:"
          print header
          let numRecords = V.length (records :: V.Vector HeroBattle)
          putStrLn $ "\nRecords:" ++ show numRecords
          print (records V.! 0)

getInputChoice :: IO Int
getInputChoice = do
  putStrLn "\nMake a choice:"
  putStrLn "    1. Check Hero data file"
  putStrLn "    2. Update Leagues (after Sunday)"
  putStrLn "    3. Get results for Guild war"
  putStrLn "    4. Find best team for Hero team"
  putStrLn "    5. Find best team for Titans team"
  putStrLn "    0. Exit"
  choice <- getLine
  case readMaybe choice of
    Just num -> do
      if num < 3 && num >= 0
        then return num
        else do
          putStrLn "Not a valid choice"
          getInputChoice
    Nothing -> do
      putStrLn "Invalid choice. Try again"
      getInputChoice

test :: IO (Either String Int)
test = do
  ff <- getLine
  case readMaybe ff :: (Maybe Int) of
    Just num -> return (Right num)
    Nothing -> return (Left (ff ++ " is an Invalid integer"))

myReadMaybe :: (Read a) => String -> Maybe a
myReadMaybe x =
  case reads x of
    [(num, "")] -> Just num
    _ -> Nothing

data Export a = Export { date :: Int , type_ :: String  , data_ :: [a]}
