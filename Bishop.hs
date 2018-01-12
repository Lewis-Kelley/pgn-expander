module Bishop where

import Board
import Types

validBishopMove :: GameState -> Color -> (Int, Int) -> (Int, Int) -> Bool
validBishopMove state color origin destination =
  validDestination state color destination
  && definesDiagonal origin destination
  && clearLine state origin destination
