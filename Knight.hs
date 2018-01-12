module Knight where

import Board
import Types

validKnightMove :: GameState -> Color -> (Int, Int) -> (Int, Int) -> Bool
validKnightMove state color origin destination =
  validDestination state color destination
  && definesKnightMove origin destination

definesKnightMove :: (Int, Int) -> (Int, Int) -> Bool
definesKnightMove (col1, row1) (col2, row2) =
  (abs (col1 - col2) == 2
   && abs (row1 - row2) == 1)
  || (abs (col1 - col2) == 1
     && abs (row1 - row2) == 2)
