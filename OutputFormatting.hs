module OutputFormatting where

import Types

-- |GameID|Outcome|White Name|Black Name|White ELO|Black ELO|Date|...
-- |GameID|Turn|Ply No.|Piece|Source|Dest|CapturedPiece|PromotionPiece|CheckState|CastleSide|

formatMoves :: [Move] -> [String]
formatMoves = map formatMoveType

formatMoveType :: Move -> String
formatMoveType (BasicMove piece origin destination
                 promotion checkState) =
  formatMove piece origin destination Nothing
  promotion checkState Nothing
formatMoveType (TakingMove piece takenPiece origin destination
                 promotion checkState) =
  formatMove piece origin destination (Just takenPiece)
  promotion checkState Nothing
--FIXME Once we have turn information
formatMoveType (CastleMove KingSide checkState) =
  formatMove (ColoredPiece White King) (4, 0) (6, 0) Nothing
  Nothing checkState (Just KingSide)
formatMoveType (CastleMove QueenSide checkState) =
  formatMove (ColoredPiece White King) (4, 0) (2, 0) Nothing
  Nothing checkState (Just QueenSide)

formatMove :: ColoredPiece -> Cell -> Cell -> Maybe ColoredPiece
  -> Promotion -> CheckState -> Maybe CastleSide -> String
formatMove movedPiece origin destination takenPiece promotion checkState castleSide =
  -- GameID
  -- Turn
  -- Ply
  formatColoredPiece movedPiece ++ "," ++
  formatCell origin ++ "," ++
  formatCell destination ++ "," ++
  formatTakenPiece takenPiece ++ "," ++
  formatPromotion promotion ++ "," ++
  formatCheckState checkState ++ "," ++
  formatCastleSide castleSide ++ "\n"

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
