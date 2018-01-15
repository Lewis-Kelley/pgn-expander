module FileHandling where

import Data.Text (Text)
import qualified Data.Text.IO as T (hGetContents)
import System.IO

import OutputFormatting
import Types

getPgnContents :: IO Text
getPgnContents = do
  putStrLn "Enter the name of the file to parse: "
  fileName <- getLine
  fileHandle <- openFile fileName ReadMode
  hSetEncoding fileHandle latin1
  T.hGetContents fileHandle

writeGames :: [Maybe ([String], [Move])] -> IO ()
writeGames = writeEgn . foldGames

foldGames :: [Maybe ([String], [Move])] -> String
foldGames games =
  let gameStrings =
        map (\ game ->
                case game of
                  Nothing -> Nothing
                  Just (keys, moves) ->
                    Just $ foldGame keys $ formatMoves moves) games
  in
    foldGameStrings gameStrings

foldGameStrings :: [Maybe String] -> String
foldGameStrings gameStrings =
  let (folded, totalErrors) =
        foldr (\ game (rest, errCount) ->
                  case game of
                    Nothing -> (rest, errCount + 1)
                    Just gameString ->
                      (gameString ++ "======\n" ++ rest, errCount))
        ("", 0) gameStrings
  in
    "ERRORS: " ++ (show totalErrors) ++ "\n\n" ++ folded

foldGame :: [String] -> [String] -> String
foldGame keys moves = (foldKeys keys) ++ "\n" ++ (foldMoves moves)

foldKeys :: [String] -> String
foldKeys = unlines

foldMoves :: [String] -> String
foldMoves = unlines

writeEgn :: String -> IO ()
writeEgn contents = do
  putStrLn "Enter the name of the output file: "
  fileName <- getLine
  writeFile fileName contents
