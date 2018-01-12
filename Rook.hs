module Rook where

import Board
import Types

validRookMove :: GameState -> Color -> (Int, Int) -> (Int, Int) -> Bool
validRookMove state color origin destination =
  validDestination state color destination
  && (definesCol origin destination
      || definesRow origin destination)
  && clearLine state origin destination
