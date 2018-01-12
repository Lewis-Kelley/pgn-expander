module OutputFormatting where

import Types

formatMoves :: [Move] -> [String]
formatMoves = map formatMove

formatMove :: Move -> String
formatMove (BasicMove piece origin destination promotion checkState) =
  formatColoredPiece piece ++ " " ++
  formatCell origin ++ "->" ++
  formatCell destination ++ " " ++
  formatPromotion promotion ++ " " ++
  formatCheckState checkState ++ "\n"
formatMove (TakingMove piece takenPiece origin destination promotion checkState) =
  formatColoredPiece piece ++ " " ++ "takes " ++
  formatColoredPiece takenPiece ++ " " ++
  formatCell origin ++ "->" ++
  formatCell destination ++ " " ++
  formatPromotion promotion ++ " " ++
  formatCheckState checkState ++ "\n"
formatMove (CastleMove side checkState) =
  "castle " ++ formatCastleSide side ++ " " ++
  formatCheckState checkState ++ "\n"

formatCell :: Cell -> String
formatCell (row, col) =
  "(" ++ formatRow row ++ "," ++ formatCol col ++ ")"

formatRow :: Int -> String
formatRow = show

formatCol :: Int -> String
formatCol = show

formatColoredPiece :: ColoredPiece -> String
formatColoredPiece NoPiece = ""
formatColoredPiece (ColoredPiece color piece) =
  formatColor color ++ " " ++ formatPiece piece

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
formatPromotion Nothing = "-"
formatPromotion (Just piece) = "^" ++ formatPiece piece

formatCheckState :: CheckState -> String
formatCheckState NoCheck = "-"
formatCheckState Check = "check"
formatCheckState CheckMate = "checkmate"

formatCastleSide :: CastleSide -> String
formatCastleSide QueenSide = "queenside"
formatCastleSide KingSide = "kingside"
