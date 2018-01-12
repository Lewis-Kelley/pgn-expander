module King where

import Board
import Types

-- Ignoring checks because we don't need to worry about them
validKingMove :: GameState -> Color -> (Int, Int) -> (Int, Int) -> Bool
validKingMove state color origin destination =
  validDestination state color destination
  && definesKingMove origin destination

definesKingMove :: (Int, Int) -> (Int, Int) -> Bool
definesKingMove (col1, row1) (col2, row2) =
  abs (col1 - col2) <= 1
  && abs (row1 - row2) <= 1
