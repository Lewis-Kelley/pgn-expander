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

data Move = BasicMove Turn Ply ColoredPiece Cell Cell Promotion CheckState
          | TakingMove Turn Ply ColoredPiece ColoredPiece Cell Cell Promotion CheckState
          | CastleMove Turn Ply CastleSide CheckState
  deriving Show

data SemiMove = SemiBasicMove Piece SemiCell Cell Promotion CheckState
              | SemiTakingMove Piece SemiCell Cell Promotion CheckState
              | SemiCastleMove CastleSide CheckState
  deriving Show

data CastleSide = QueenSide | KingSide
  deriving (Eq, Show)

data GameState = GameState Turn Ply [[ColoredPiece]] PieceMap
  deriving (Show)

data Ply = Ply Int
  deriving (Show)

data GameID = GameID Int
  deriving Show

type Turn = Color
type PieceMap = Map ColoredPiece [Cell]

data KeyValMap = KeyValMap (Map String String)
