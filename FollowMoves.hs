module FollowMoves where

import Data.Map.Strict as Map hiding (filter, null)
import Bishop
import Board
import King
import Knight
import Pawn
import Rook
import Types
import Util
import UpdateState

initialGameState :: GameState
initialGameState =
  GameState White makeInitBoard makeInitPieceMap

makeInitBoard :: [[ColoredPiece]]
makeInitBoard =
  [initialHomeRow White,
   replicate 8 $ ColoredPiece White Pawn,
   replicate 8 NoPiece,
   replicate 8 NoPiece,
   replicate 8 NoPiece,
   replicate 8 NoPiece,
   replicate 8 $ ColoredPiece Black Pawn,
   initialHomeRow Black]

makeInitPieceMap :: PieceMap
makeInitPieceMap =
  fromList [(ColoredPiece White Pawn,   [(0, 1), (1, 1),
                                         (2, 1), (3, 1),
                                         (4, 1), (5, 1),
                                         (6, 1), (7, 1)]),
            (ColoredPiece White Rook,   [(0, 0), (7, 0)]),
            (ColoredPiece White Knight, [(1, 0), (6, 0)]),
            (ColoredPiece White Bishop, [(2, 0), (5, 0)]),
            (ColoredPiece White Queen,  [(3, 0)]),
            (ColoredPiece White King,   [(4, 0)]),
            (ColoredPiece Black Pawn,   [(0, 6), (1, 6),
                                         (2, 6), (3, 6),
                                         (4, 6), (5, 6),
                                         (6, 6), (7, 6)]),
            (ColoredPiece Black Rook,   [(0, 7), (7, 7)]),
            (ColoredPiece Black Knight, [(1, 7), (6, 7)]),
            (ColoredPiece Black Bishop, [(2, 7), (5, 7)]),
            (ColoredPiece Black Queen,  [(3, 7)]),
            (ColoredPiece Black King,   [(4, 7)])]

initialHomeRow :: Color -> [ColoredPiece]
initialHomeRow color =
  [ColoredPiece color Rook, ColoredPiece color Knight,
   ColoredPiece color Bishop, ColoredPiece color Queen,
   ColoredPiece color King, ColoredPiece color Bishop,
   ColoredPiece color Knight, ColoredPiece color Rook]

pawnRow :: Color -> [ColoredPiece]
pawnRow color = fullRow $ ColoredPiece color Pawn

emptyRow :: [ColoredPiece]
emptyRow = fullRow NoPiece

fullRow :: ColoredPiece -> [ColoredPiece]
fullRow = replicate 8

followMoves :: ([Move] -> [Move]) -> GameState -> [SemiMove] -> [Move]
followMoves continuation state semiMoves =
  if null semiMoves
  then continuation []
  else
    let (fullMove, nextState) = followMove state $ head semiMoves
        nextContinuation = attachContinuation continuation fullMove in
      followMoves nextContinuation nextState $ tail semiMoves

followMove :: GameState -> SemiMove -> (Move, GameState)
followMove state (SemiBasicMove piece origin
                   destination promotion checkState) =
  let coloredPiece = getColoredPiece state piece
      originOptions = getValidPieceOrigins state coloredPiece destination
      fullOrigin = choosePieceOrigin origin originOptions
      move = BasicMove coloredPiece fullOrigin destination promotion checkState
  in
    (move, updateState state move)
followMove state (SemiTakingMove piece origin
                   destination promotion checkState) =
  let coloredPiece = getColoredPiece state piece
      originOptions = getValidPieceOrigins state coloredPiece destination
      fullOrigin = choosePieceOrigin origin originOptions
      takenPiece = getPieceAt state destination
      move = (TakingMove coloredPiece takenPiece fullOrigin
              destination promotion checkState)
  in
    (move, updateState state move)
followMove state (SemiCastleMove castleSide checkState) =
  let move = (CastleMove castleSide checkState)
  in
    (move, updateState state move)

getColoredPiece :: GameState -> Piece -> ColoredPiece
getColoredPiece (GameState turn _ _) piece = ColoredPiece turn piece

getValidPieceOrigins :: GameState -> ColoredPiece -> Cell -> [Cell]
getValidPieceOrigins state coloredPiece destination =
  let pieceLocations = getPieceLocations state coloredPiece
  in filter (validMoveFrom state coloredPiece destination) pieceLocations

getPieceLocations :: GameState -> ColoredPiece -> [Cell]
getPieceLocations (GameState _ _ pieceMap) piece =
  case Map.lookup piece pieceMap of
    Nothing -> []
    Just locations -> locations

validMoveFrom :: GameState -> ColoredPiece -> Cell -> Cell -> Bool
validMoveFrom _ NoPiece _ _ = False
validMoveFrom state (ColoredPiece color piece) destination origin =
  if not $ validDestination state color destination then False
  else
    case piece of
      Pawn -> validPawnMove state color destination origin
      Rook -> validRookMove state color destination origin
      Knight -> validKnightMove state color destination origin
      Bishop -> validBishopMove state color destination origin
      Queen -> validRookMove state color destination origin
               || validBishopMove state color destination origin
      King -> validKingMove state color destination origin

choosePieceOrigin :: SemiCell -> [Cell] -> Cell
choosePieceOrigin (col, row) origins =
  if length origins == 1
  then head origins
  else
    case col of
      Just actCol ->
        choosePieceOrigin (Nothing, row)
        (filter (\ (origCol, _) ->
                    origCol == actCol)
          origins)
      Nothing ->
        case row of
          Just actRow ->
            choosePieceOrigin (col, Nothing)
            (filter (\ (_, origRow) ->
                        origRow == actRow)
             origins)
          Nothing -> error "No origin found"

getPieceAt :: GameState -> Cell -> ColoredPiece
getPieceAt (GameState _ board _) (col, row) = ((board !! col) !! row)
