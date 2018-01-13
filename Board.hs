module Board where

import Types

pieceColorAtPosIs :: GameState -> Cell -> Maybe Color -> Bool
pieceColorAtPosIs state position color =
  color == pieceColorAtPos state position

pieceColorAtPos :: GameState -> Cell -> Maybe Color
pieceColorAtPos state position =
  case pieceAtPos state position of
    NoPiece -> Nothing
    ColoredPiece color _ -> Just color

pieceAtPos :: GameState -> Cell -> ColoredPiece
pieceAtPos (GameState _ board _) (col, row) =
  (board !! row) !! col

validDestination :: GameState -> Color -> Cell -> Bool
validDestination state color destination =
  not $ pieceColorAtPosIs state destination $ Just color

clearLine :: GameState -> Cell -> Cell -> Bool
clearLine state pt1 pt2 =
  case pointsBetween pt1 pt2 of
    Nothing -> False
    Just pointsList -> areEmpty state pointsList

pointsBetween :: Cell -> Cell -> Maybe [Cell]
pointsBetween pt1@(pt1Col, pt1Row) pt2@(pt2Col, pt2Row)
  | definesRow pt1 pt2 = Just $ makeRow pt1Col pt1Row pt2Row
  | definesCol pt1 pt2 = Just $ makeCol pt1Row pt1Col pt2Col
  | definesDiagonal pt1 pt2 = Just $ makeDiagonal pt1Col pt1Row pt2Col pt2Row
  | otherwise = Nothing

definesRow :: Cell -> Cell -> Bool
definesRow (pt1Col, pt1Row) (pt2Col, pt2Row) =
  pt1Col /= pt2Col && pt1Row == pt2Row

makeRow :: Int -> Int -> Int -> [Cell]
makeRow row col1 col2 =
  let dist = abs(col1 - col2) - 1
      base = min col1 col2
  in
    map (\ index -> (base + index, row)) [1..dist]

makeCol :: Int -> Int -> Int -> [Cell]
makeCol col row1 row2 =
  let dist = abs(row1 - row2) - 1
      base = min row1 row2
  in
    map (\ index -> (col, base + index)) [1..dist]

  let dist = abs(row1 - row2) - 1
      baseRow = min row1 row2
      baseCol = min col1 col2
  in
    map (\ index -> (baseCol + index, baseRow + index)) [1..dist]

definesCol :: (Int, Int) -> (Int, Int) -> Bool
makeDiagonal :: Int -> Int -> Int -> Int -> [Cell]
makeDiagonal col1 row1 col2 row2

definesCol :: Cell -> Cell -> Bool
definesCol (pt1Col, pt1Row) (pt2Col, pt2Row) =
  pt1Col == pt2Col && pt1Row /= pt2Row

definesDiagonal :: Cell -> Cell -> Bool
definesDiagonal (pt1Col, pt1Row) (pt2Col, pt2Row) =
  abs (pt2Col - pt1Col) == abs(pt2Row - pt1Row)

areEmpty :: GameState -> [Cell] -> Bool
areEmpty state positions =
  null $ filter (\ position ->
                    not $ pieceColorAtPosIs state position Nothing)
  positions

moveInBoard :: [[ColoredPiece]] -> ColoredPiece -> Cell -> Cell -> [[ColoredPiece]]
moveInBoard board piece origin destination =
  setInBoard (setInBoard board origin NoPiece) destination piece

setInBoard :: [[ColoredPiece]] -> Cell -> ColoredPiece -> [[ColoredPiece]]
setInBoard board (col, row) newVal =
  foldr (\ colData rest ->
           let colNumber = 7 - length rest
           in
             if colNumber == col
             then (setInColumn colData row newVal : rest)
             else (colData : rest)) [] board

setInColumn :: [ColoredPiece] -> Int -> ColoredPiece -> [ColoredPiece]
setInColumn column row newVal =
  foldr (\ rowElem rest ->
           let rowNumber = 7 - length rest
           in
             if rowNumber == row
             then (newVal : rest)
             else (rowElem : rest)) [] column
