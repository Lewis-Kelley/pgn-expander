module Cell where

import Types
import Util

parseCell :: String -> Maybe (Cell, String)
parseCell [] = Nothing
parseCell (_ : []) = Nothing
parseCell moveString =
  let colChar = (moveString !! 0)
      rowChar = (moveString !! 1) in
    case charToCol colChar of
      Nothing -> Nothing
      Just column ->
        case charToRow rowChar of
          Nothing -> Nothing
          Just row ->
            Just ((column, row), drop 2 moveString)

isCell :: String -> Bool
isCell (colChar : rowChar : "") = isColChar colChar && isRowChar rowChar
isCell _ = False

isColChar :: Char -> Bool
isColChar = notNothing . charToCol

isRowChar :: Char -> Bool
isRowChar = notNothing . charToRow

cellToString :: Cell -> String
cellToString (row, col) = [rowToChar row, colToChar col]

rowToChar :: Int -> Char
rowToChar 0 = '1'
rowToChar 1 = '2'
rowToChar 2 = '3'
rowToChar 3 = '4'
rowToChar 4 = '5'
rowToChar 5 = '6'
rowToChar 6 = '7'
rowToChar 7 = '8'
rowToChar _ = 'X'

charToRow :: Char -> Maybe Int
charToRow '1' = Just 0
charToRow '2' = Just 1
charToRow '3' = Just 2
charToRow '4' = Just 3
charToRow '5' = Just 4
charToRow '6' = Just 5
charToRow '7' = Just 6
charToRow '8' = Just 7
charToRow _ = Nothing

colToChar :: Int -> Char
colToChar 0 = 'a'
colToChar 1 = 'b'
colToChar 2 = 'c'
colToChar 3 = 'd'
colToChar 4 = 'e'
colToChar 5 = 'f'
colToChar 6 = 'g'
colToChar 7 = 'h'
colToChar _ = 'X'

charToCol :: Char -> Maybe Int
charToCol 'a' = Just 0
charToCol 'b' = Just 1
charToCol 'c' = Just 2
charToCol 'd' = Just 3
charToCol 'e' = Just 4
charToCol 'f' = Just 5
charToCol 'g' = Just 6
charToCol 'h' = Just 7
charToCol _ = Nothing
