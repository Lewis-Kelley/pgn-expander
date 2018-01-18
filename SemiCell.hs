module SemiCell where

import Cell
import Types

parseSemiCell :: String -> (SemiCell, String)
parseSemiCell moveString =
  let (column, afterColumn) = parseSemiColumn moveString
      (row, afterRow) = parseSemiRow afterColumn in
    ((column, row), afterRow)

parseSemiColumn :: String -> (Maybe Int, String)
parseSemiColumn = parseSemiCoord charToCol

parseSemiRow :: String -> (Maybe Int, String)
parseSemiRow = parseSemiCoord charToRow

parseSemiCoord :: (Char -> Maybe Int) -> String -> (Maybe Int, String)
parseSemiCoord _ [] = (Nothing, "")
parseSemiCoord charToCoord moveString =
  case charToCoord $ head moveString of
    Nothing -> (Nothing, moveString)
    Just coord -> (Just coord, tail moveString)
