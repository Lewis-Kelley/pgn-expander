module ParseMoves where

import Cell
import Piece
import SemiCell
import Types
import Util

parseMoves :: [String] -> [SemiMove]
parseMoves body =
  let moves = getMoveStrings body
  in unMaybeList $ map parseMove moves

getMoveStrings :: [String] -> [String]
getMoveStrings = removeNumbers . words . unwords

removeNumbers :: [String] -> [String]
removeNumbers separateMoves = filter (not . elem '.') separateMoves

parseMove :: String -> Maybe SemiMove
parseMove move
  | isCastle move = getCastle move
  | isTaking move = getTaking move
  | otherwise = getBasic move

isCastle :: String -> Bool
isCastle ('O' : '-' : 'O' : _) = True
isCastle _ = False

isTaking :: String -> Bool
isTaking = elem 'x'

getCastle :: String -> Maybe SemiMove
getCastle ('O' : '-' : 'O' : '-' : 'O' : rest) =
  makeCastle QueenSide rest
getCastle ('O' : '-' : 'O' : rest) =
  makeCastle KingSide rest
getCastle _ = Nothing

makeCastle :: CastleSide -> String -> Maybe SemiMove
makeCastle castleSide rest =
  let (checkState, afterCheckState) = stringToCheckState rest in
    if removeSANSuffixes afterCheckState /= ""
    then Nothing
    else Just $ SemiCastleMove castleSide checkState

getTaking :: String -> Maybe SemiMove
getTaking moveString =
  case parsePiece moveString of
    Nothing -> Nothing
    Just (piece, afterPiece) ->
      let (origin, afterOrigin) = parseSemiCell afterPiece in
        if null afterOrigin || head afterOrigin /= 'x' then Nothing
        else let afterTake = tail afterOrigin in
               if not $ isCell $ take 2 afterTake then Nothing
               else finishGetMove SemiTakingMove piece origin afterTake

getBasic :: String -> Maybe SemiMove
getBasic moveString =
  case parsePiece moveString of
    Nothing -> Nothing
    Just (piece, afterPiece) ->
      let (origin, afterOrigin) = parseSemiCell afterPiece in
        if isCell $ take 2 afterOrigin
        then finishGetMove SemiBasicMove piece origin afterOrigin
        else finishGetMove SemiBasicMove piece (Nothing, Nothing) afterPiece

type MoveBuilder = Piece -> SemiCell -> Cell -> Promotion -> CheckState -> SemiMove

finishGetMove :: MoveBuilder -> Piece -> SemiCell -> String -> Maybe SemiMove
finishGetMove moveBuilder piece origin moveString =
  case parseCell moveString of
    Nothing -> Nothing
    Just (destination, afterDestination) ->
      case stringToPromotion afterDestination of
        Nothing -> Nothing
        Just (promotion, afterPromotion) ->
          let (checkState, afterCheck) = stringToCheckState afterPromotion in
            if removeSANSuffixes afterCheck /= ""
            then Nothing
            else Just $ moveBuilder piece origin destination promotion checkState

stringToPromotion :: String -> Maybe (Promotion, String)
stringToPromotion ('=' : pieceChar : rest)
  | isPieceChar pieceChar = Just (charToPiece pieceChar, rest)
  | otherwise = Nothing
stringToPromotion rest = Just (Nothing, rest)

stringToCheckState :: String -> (CheckState, String)
stringToCheckState ('+' : rest) = (Check, rest)
stringToCheckState ('#' : rest) = (CheckMate, rest)
stringToCheckState rest = (NoCheck, rest)

removeSANSuffixes :: String -> String
removeSANSuffixes ('?' : '?' : rest) = rest
removeSANSuffixes ('!' : '?' : rest) = rest
removeSANSuffixes ('?' : '!' : rest) = rest
removeSANSuffixes ('!' : '!' : rest) = rest
removeSANSuffixes ('!' : rest) = rest
removeSANSuffixes ('?' : rest) = rest
removeSANSuffixes rest = rest
