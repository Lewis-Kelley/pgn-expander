module OutputFormatting where

import Types

-- |GameID|Outcome|White Name|Black Name|White ELO|Black ELO|Date|...
-- |GameID|Turn|Ply No.|Piece|Source|Dest|CapturedPiece|PromotionPiece|CheckState|CastleSide|

formatMoves :: [Move] -> [String]
formatMoves = map formatMoveType

formatMoveType :: Move -> String
formatMoveType (BasicMove gameId turn ply piece origin destination
                 promotion checkState) =
  formatMove gameId turn ply piece origin destination Nothing
  promotion checkState Nothing
formatMoveType (TakingMove gameId turn ply piece takenPiece origin destination
                 promotion checkState) =
  formatMove gameId turn ply piece origin destination (Just takenPiece)
  promotion checkState Nothing
formatMoveType (CastleMove gameId turn ply KingSide checkState) =
  formatMove gameId turn ply (ColoredPiece turn King) (4, 0) (6, 0) Nothing
  Nothing checkState (Just KingSide)
formatMoveType (CastleMove gameId turn ply QueenSide checkState) =
  formatMove gameId turn ply (ColoredPiece turn King) (4, 0) (2, 0) Nothing
  Nothing checkState (Just QueenSide)

formatMove :: GameID -> Turn -> Ply -> ColoredPiece -> Cell -> Cell
  -> Maybe ColoredPiece -> Promotion -> CheckState -> Maybe CastleSide -> String
formatMove gameId turn ply movedPiece origin destination takenPiece promotion checkState castleSide =
  formatGameId gameId ++ "," ++
  formatTurn turn ++ "," ++
  formatPly ply ++ "," ++
  formatColoredPiece movedPiece ++ "," ++
  formatCell origin ++ "," ++
  formatCell destination ++ "," ++
  formatTakenPiece takenPiece ++ "," ++
  formatPromotion promotion ++ "," ++
  formatCheckState checkState ++ "," ++
  formatCastleSide castleSide

formatGameId :: GameID -> String
formatGameId (GameID idNum) = show idNum

formatTurn :: Turn -> String
formatTurn = formatColor

formatColor :: Color -> String
formatColor White = "w"
formatColor Black = "b"

formatPly :: Ply -> String
formatPly (Ply plyNum) = show plyNum

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
