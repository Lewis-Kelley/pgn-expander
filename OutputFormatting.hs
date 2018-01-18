module OutputFormatting where

import Types

-- |GameID|Outcome|White Name|Black Name|White ELO|Black ELO|Date|...
-- |GameID|Turn|Ply No.|Piece|Source|Dest|CapturedPiece|PromotionPiece|CheckState|CastleSide|

formatMoves :: [Move] -> [String]
formatMoves = map formatMoveType

formatMoveType :: Move -> String
formatMoveType (BasicMove gameId piece origin destination
                 promotion checkState) =
  formatMove gameId piece origin destination Nothing
  promotion checkState Nothing
formatMoveType (TakingMove gameId piece takenPiece origin destination
                 promotion checkState) =
  formatMove gameId piece origin destination (Just takenPiece)
  promotion checkState Nothing
--FIXME Once we have turn information
formatMoveType (CastleMove gameId KingSide checkState) =
  formatMove gameId (ColoredPiece White King) (4, 0) (6, 0) Nothing
  Nothing checkState (Just KingSide)
formatMoveType (CastleMove gameId QueenSide checkState) =
  formatMove gameId (ColoredPiece White King) (4, 0) (2, 0) Nothing
  Nothing checkState (Just QueenSide)

formatMove :: GameID -> ColoredPiece -> Cell -> Cell -> Maybe ColoredPiece
  -> Promotion -> CheckState -> Maybe CastleSide -> String
formatMove gameId movedPiece origin destination takenPiece promotion checkState castleSide =
  formatGameId gameId ++ "," ++
  -- Turn
  -- Ply
  formatColoredPiece movedPiece ++ "," ++
  formatCell origin ++ "," ++
  formatCell destination ++ "," ++
  formatTakenPiece takenPiece ++ "," ++
  formatPromotion promotion ++ "," ++
  formatCheckState checkState ++ "," ++
  formatCastleSide castleSide ++ "\n"

formatGameId :: GameID -> String
formatGameId (GameID idNum) = show idNum

formatCell :: Cell -> String
formatCell (row, col) =
  formatRow row ++ "," ++ formatCol col

formatRow :: Int -> String
formatRow = show

formatCol :: Int -> String
formatCol = show

formatTakenPiece :: Maybe ColoredPiece -> String
formatTakenPiece Nothing = ""
formatTakenPiece (Just NoPiece) = ""
formatTakenPiece (Just piece@(ColoredPiece _ _)) = formatColoredPiece piece

formatColoredPiece :: ColoredPiece -> String
formatColoredPiece NoPiece = ""
formatColoredPiece (ColoredPiece White Pawn) = "P"
formatColoredPiece (ColoredPiece Black Pawn) = "p"
formatColoredPiece (ColoredPiece White Rook) = "R"
formatColoredPiece (ColoredPiece Black Rook) = "r"
formatColoredPiece (ColoredPiece White Knight) = "N"
formatColoredPiece (ColoredPiece Black Knight) = "n"
formatColoredPiece (ColoredPiece White Bishop) = "B"
formatColoredPiece (ColoredPiece Black Bishop) = "b"
formatColoredPiece (ColoredPiece White Queen) = "Q"
formatColoredPiece (ColoredPiece Black Queen) = "q"
formatColoredPiece (ColoredPiece White King) = "K"
formatColoredPiece (ColoredPiece Black King) = "k"

formatColor :: Color -> String
formatColor White = "white"
formatColor Black = "black"

formatPiece :: Piece -> String
formatPiece Pawn = "pawn"
formatPiece Rook = "rook"
formatPiece Knight = "knight"
formatPiece Bishop = "bishop"
formatPiece Queen = "queen"
formatPiece King = "king"

formatPromotion :: Promotion -> String
formatPromotion Nothing = ""
formatPromotion (Just piece) = formatPiece piece

formatCheckState :: CheckState -> String
formatCheckState NoCheck = ""
formatCheckState Check = "check"
formatCheckState CheckMate = "checkmate"

formatCastleSide :: Maybe CastleSide -> String
formatCastleSide Nothing = ""
formatCastleSide (Just QueenSide) = "q"
formatCastleSide (Just KingSide) = "k"
