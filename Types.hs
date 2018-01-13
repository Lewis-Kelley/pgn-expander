module Types where

import Data.Map.Strict

type Cell = (Int, Int) -- Column, Row
type SemiCell = (Maybe Int, Maybe Int)

data CheckState = NoCheck | Check | CheckMate
  deriving Show

data Piece = Pawn
           | Rook
           | Knight
           | Bishop
           | Queen
           | King
  deriving (Eq, Ord, Show)

data Color = White | Black
  deriving (Eq, Ord, Show)

data ColoredPiece = ColoredPiece Color Piece
                  | NoPiece
  deriving (Eq, Ord, Show)

type Promotion = Maybe Piece

data Move = BasicMove ColoredPiece Cell Cell Promotion CheckState
          | TakingMove ColoredPiece ColoredPiece Cell Cell Promotion CheckState
          | CastleMove CastleSide CheckState
  deriving Show

data SemiMove = SemiBasicMove Piece SemiCell Cell Promotion CheckState
              | SemiTakingMove Piece SemiCell Cell Promotion CheckState
              | SemiCastleMove CastleSide CheckState
  deriving Show

data CastleSide = QueenSide | KingSide
  deriving (Eq, Show)

data GameState = GameState Turn [[ColoredPiece]] PieceMap
  deriving (Show)

type Turn = Color
type PieceMap = Map ColoredPiece [Cell]
