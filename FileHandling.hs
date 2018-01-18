module FileHandling where

import System.Environment
import Data.Text (Text)
import qualified Data.Text.IO as T (hGetContents)
import System.IO

import OutputFormatting
import Types
import Util

getPgnContents :: IO Text
getPgnContents = do
  args <- getArgs
  let baseFileName = getFileName args
  fileHandle <- openFile (baseFileName ++ ".pgn") ReadMode
  hSetEncoding fileHandle latin1
  T.hGetContents fileHandle

getFileName :: [String] -> String
getFileName args = do
  if (length args) < 1
    then error "Usage: ./Chess <input pgn without extension>"
    else args !! 0


writeGames :: [Maybe ([String], [Move])] -> IO ()
writeGames gameData = do
  let (contents, gameErrorCounts) = foldGames gameData
  writeCsv contents
  -- writeGameCsv gameContents
  -- writeKeyCsv keyContents
  writeAccuracy gameErrorCounts

foldGames :: [Maybe ([String], [Move])] -> (String, (Int, Int))
foldGames games =
  let rawKeyGameStrings =
        map (\ game ->
                case game of
                  Nothing -> Nothing
                  Just (keys, moves) ->
                    Just (foldKeys keys
                           ++ (foldMoves . formatMoves) moves)) games
      totalLength = length rawKeyGameStrings
      gameStrings = unMaybeList rawKeyGameStrings
      finalLength = length gameStrings
  in
    (foldGameStrings gameStrings, (totalLength, finalLength))

foldGameStrings :: [String] -> String
foldGameStrings = foldr (++) ""

foldKeys :: [String] -> String
foldKeys keyLines = unlines $ map (\line -> ">" ++ line) keyLines

foldGameKeys :: [String] -> String
foldGameKeys = foldr (++) ""

foldMoves :: [String] -> String
foldMoves moveLines = unlines $ map (\ line -> "<" ++ line) moveLines

writeCsv :: String -> IO ()
writeCsv contents = do
  args <- getArgs
  let baseFileName = getFileName args
  writeFile (baseFileName ++ ".csv") contents

writeGameCsv :: String -> IO ()
writeGameCsv contents = do
  args <- getArgs
  let baseFileName = getFileName args
  writeFile (baseFileName ++ ".csv") contents

writeKeyCsv :: String -> IO ()
writeKeyCsv contents = do
  args <- getArgs
  let baseFileName = getFileName args
  writeFile (baseFileName ++ "_keys.csv") contents

writeAccuracy :: (Int, Int) -> IO ()
writeAccuracy (totalGames, goodGames) = do
  args <- getArgs
  let baseFileName = getFileName args
  writeFile (baseFileName ++ "_info.txt")
    ("Total: " ++ show totalGames
     ++ "\nErrors: " ++ show (totalGames - goodGames))
