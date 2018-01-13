module Pawn where

import Board
import Types

validPawnMove :: GameState -> Color -> Cell -> Cell -> Bool
validPawnMove state color origin dest =
  validDoubleMove state color origin dest
  || validSingleMove state color origin dest
  || validTakingMove state color origin dest

validDoubleMove :: GameState -> Color -> Cell -> Cell -> Bool
validDoubleMove state color (originCol, originRow) (destCol, destRow)
  | color == White = originCol == destCol
                     && destRow - originRow == 2
                     && originRow == 1
                     && pieceColorAtPosIs state (destCol, destRow) Nothing
                     && pieceColorAtPosIs state (destCol, destRow - 1) Nothing
  | otherwise      = originCol == destCol
                     && originRow - destRow == 2
                     && originRow == 6
                     && pieceColorAtPosIs state (destCol, destRow) Nothing
                     && pieceColorAtPosIs state (destCol, destRow + 1) Nothing

validSingleMove :: GameState -> Color -> Cell -> Cell -> Bool
validSingleMove state color (originCol, originRow) (destCol, destRow)
  | color == White = originCol == destCol
                     && destRow - originRow == 1
                     && pieceColorAtPosIs state (destCol, destRow) Nothing
  | otherwise      = originCol == destCol
                     && originRow - destRow == 1
                     && pieceColorAtPosIs state (destCol, destRow) Nothing

-- TODO Add En Passant Support
validTakingMove :: GameState -> Color -> (Int, Int) -> (Int, Int) -> Bool
validTakingMove state color (originCol, originRow) (destCol, destRow)
  | color == White = abs (originCol - destCol) == 1
                     && destRow - originRow == 1
                     && pieceColorAtPosIs state (destCol, destRow) (Just Black)
  | otherwise      = abs (originCol - destCol) == 1
                     && originRow - destRow == 1
                     && pieceColorAtPosIs state (destCol, destRow) (Just White)
