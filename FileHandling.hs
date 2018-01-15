module FileHandling where

import OutputFormatting
import Types

getPgnContents :: IO String
getPgnContents = do
  putStrLn "Enter the name of the file to parse: "
  fileName <- getLine
  readFile fileName

writeGames :: [Maybe ([String], [Move])] -> IO ()
writeGames = writeEgn . foldGames

foldGames :: [Maybe ([String], [Move])] -> String
foldGames games =
  let gameStrings =
        map (\ game ->
                case game of
                  Nothing -> ""
                  Just (keys, moves) ->
                    foldGame keys $ formatMoves moves) games
  in
    unlines gameStrings

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
