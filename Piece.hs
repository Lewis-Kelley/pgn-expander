module Piece where

import Data.Char
import Types
import Util

parsePiece :: String -> Maybe (Piece, String)
parsePiece move =
  if not $ isUpper $ head move
  then Just (Pawn, move)
  else case charToPiece $ head $ move of
         Nothing -> Nothing
         Just piece -> Just (piece, tail move)

isPieceChar :: Char -> Bool
isPieceChar = notNothing . charToPiece

pieceToChar :: Piece -> Char
pieceToChar Pawn = 'P'
pieceToChar Rook = 'R'
pieceToChar Knight = 'N'
pieceToChar Bishop = 'B'
pieceToChar Queen = 'Q'
pieceToChar King = 'K'

charToPiece :: Char -> Maybe Piece
charToPiece 'R' = Just Rook
charToPiece 'N' = Just Knight
charToPiece 'B' = Just Bishop
charToPiece 'Q' = Just Queen
charToPiece 'K' = Just King
charToPiece _ = Nothing
