module UpdateState where

import Data.List (delete)
import Data.Map as Map (lookup, insert)
import Board
import Types

updateState :: GameState -> Move -> GameState
updateState lastState (BasicMove piece origin destination promotion _) =
  let unpromotedState =
        movePiece lastState piece origin destination
      promotedState =
        promoteIfNeeded unpromotedState destination promotion
  in flipTurn promotedState
updateState lastState (TakingMove piece takenPiece origin destination promotion _) =
  let pieceTakenState =
        removePiece lastState destination takenPiece
      unpromotedState =
        movePiece pieceTakenState piece origin destination
      promotedState =
        promoteIfNeeded unpromotedState destination promotion
  in flipTurn promotedState
updateState lastState (CastleMove side _) =
  flipTurn $ castleUpdate lastState side

promoteIfNeeded :: GameState -> Cell  -> Promotion -> GameState
promoteIfNeeded state _ Nothing = state
promoteIfNeeded (GameState turn board pieceMap) cell (Just piece) =
  let removedPawnPieceMap =
        removeFromPieceMap pieceMap (ColoredPiece turn Pawn) cell
      updatedPieceMap =
        addToPieceMap removedPawnPieceMap (ColoredPiece turn piece) cell
      updatedBoard =
        setInBoard board cell (ColoredPiece turn piece)
  in GameState turn updatedBoard updatedPieceMap

castleUpdate :: GameState -> CastleSide -> GameState
castleUpdate state@(GameState White _ _) QueenSide =
  movePiece (movePiece state (ColoredPiece White King) (4, 0) (2, 0))
  (ColoredPiece White Rook) (0, 0) (3, 0)
castleUpdate state@(GameState White _ _) KingSide =
  movePiece (movePiece state (ColoredPiece White King) (4, 0) (6, 0))
  (ColoredPiece White Rook) (7, 0) (5, 0)
castleUpdate state@(GameState Black _ _) QueenSide =
  movePiece (movePiece state (ColoredPiece Black King) (4, 7) (2, 7))
  (ColoredPiece White Rook) (0, 7) (3, 7)
castleUpdate state@(GameState Black _ _) KingSide =
  movePiece (movePiece state (ColoredPiece Black King) (4, 7) (6, 7))
  (ColoredPiece White Rook) (7, 7) (5, 7)


movePiece :: GameState -> ColoredPiece -> Cell -> Cell -> GameState
movePiece (GameState turn board pieceMap) piece origin destination =
  (GameState turn (moveInBoard board piece origin destination)
    (moveInPieceMap pieceMap piece origin destination))

removePiece :: GameState -> Cell -> ColoredPiece -> GameState
removePiece (GameState turn board pieceMap) cell piece =
  (GameState turn (setInBoard board cell piece)
    (removeFromPieceMap pieceMap piece cell))

moveInPieceMap :: PieceMap -> ColoredPiece -> Cell -> Cell -> PieceMap
moveInPieceMap pieceMap piece origin destination =
  addToPieceMap (removeFromPieceMap pieceMap piece origin) piece destination

addToPieceMap :: PieceMap -> ColoredPiece -> Cell -> PieceMap
addToPieceMap pieceMap piece cell =
  case Map.lookup piece pieceMap of
    Nothing -> error "Couldn't find piece in pieceMap"
    Just cellList ->
      Map.insert piece (cell : cellList) pieceMap

removeFromPieceMap :: PieceMap -> ColoredPiece -> Cell -> PieceMap
removeFromPieceMap pieceMap piece cell =
  case Map.lookup piece pieceMap of
    Nothing -> error "Couldn't find piece in pieceMap"
    Just cellList ->
      Map.insert piece (delete cell cellList) pieceMap

flipTurn :: GameState -> GameState
flipTurn (GameState White board pieceMap) = GameState Black board pieceMap
flipTurn (GameState Black board pieceMap) = GameState White board pieceMap
