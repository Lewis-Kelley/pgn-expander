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
  GameState White (Ply 1) makeInitBoard makeInitPieceMap

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

followMoves :: ([Move] -> [Move]) -> GameID -> GameState -> [SemiMove] -> Maybe [Move]
followMoves continuation gameId state semiMoves =
  if null semiMoves
  then Just $ continuation []
  else
    (followMove gameId state $ head semiMoves) >>=
    (\ (fullMove, nextState) ->
       let nextContinuation = attachContinuation continuation fullMove in
         followMoves nextContinuation gameId nextState $ tail semiMoves)

followMove :: GameID -> GameState -> SemiMove -> Maybe (Move, GameState)
followMove gameId state (SemiBasicMove piece origin
                          destination promotion checkState) =
  let coloredPiece = getColoredPiece state piece
      originOptions = getValidPieceOrigins state coloredPiece destination
      turn = getTurn state
      ply = getPly state
  in
    choosePieceOrigin origin originOptions >>=
    (\ fullOrigin ->
        let fullmove =
              BasicMove gameId turn ply coloredPiece fullOrigin destination promotion checkState
        in
          Just (fullmove, updateState state fullmove))

followMove gameId state (SemiTakingMove piece origin
                          destination promotion checkState) =
  let coloredPiece = getColoredPiece state piece
      originOptions = getValidPieceOrigins state coloredPiece destination
      takenPiece = getTakenPiece state destination
      turn = getTurn state
      ply = getPly state
  in
    choosePieceOrigin origin originOptions >>=
    (\ fullOrigin ->
       let fullmove =
              (TakingMove gameId turn ply coloredPiece takenPiece fullOrigin
                destination promotion checkState)
        in
          Just (fullmove, updateState state fullmove))

followMove gameId state (SemiCastleMove castleSide checkState) =
  let turn = getTurn state
      ply = getPly state
      move = (CastleMove gameId turn ply castleSide checkState)
  in
    Just (move, updateState state move)

showError :: GameState -> SemiMove -> [Cell] -> a
showError state move originOptions =
  error $ "No origin found from" ++ (show originOptions)
  ++ " at move " ++ (show move) ++ " during state " ++ (show state)

getColoredPiece :: GameState -> Piece -> ColoredPiece
getColoredPiece (GameState turn _ _ _) piece = ColoredPiece turn piece

getTurn :: GameState -> Turn
getTurn (GameState turn _ _ _) = turn

getPly :: GameState -> Ply
getPly (GameState _ ply _ _) = ply

getValidPieceOrigins :: GameState -> ColoredPiece -> Cell -> [Cell]
getValidPieceOrigins state coloredPiece destination =
  let pieceLocations = getPieceLocations state coloredPiece
  in filter (validMoveFrom state coloredPiece destination) pieceLocations

getPieceLocations :: GameState -> ColoredPiece -> [Cell]
getPieceLocations (GameState _ _ _ pieceMap) piece =
  case Map.lookup piece pieceMap of
    Nothing -> []
    Just locations -> locations

validMoveFrom :: GameState -> ColoredPiece -> Cell -> Cell -> Bool
validMoveFrom _ NoPiece _ _ = False
validMoveFrom state (ColoredPiece color piece) destination origin =
  if not $ validDestination state color destination then False
  else
    case piece of
      Pawn -> validPawnMove state color origin destination
      Rook -> validRookMove state color origin destination
      Knight -> validKnightMove state color origin destination
      Bishop -> validBishopMove state color origin destination
      Queen -> validRookMove state color origin destination
               || validBishopMove state color origin destination
      King -> validKingMove state color origin destination

choosePieceOrigin :: SemiCell -> [Cell] -> Maybe Cell
choosePieceOrigin (col, row) origins =
  if length origins == 1
  then Just $ head origins
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
          Nothing -> Nothing

getTakenPiece :: GameState -> Cell -> ColoredPiece
getTakenPiece state cell =
  case pieceAtPos state cell of
    NoPiece -> getEnPassantPiece state
    coloredPiece -> coloredPiece

-- We'll just assume that it's working correctly.
-- `validMoveFrom` should have already checked everything by now.
getEnPassantPiece :: GameState -> ColoredPiece
getEnPassantPiece (GameState White _ _ _) = (ColoredPiece Black Pawn)
getEnPassantPiece (GameState Black _ _ _) = (ColoredPiece White Pawn)
