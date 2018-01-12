import Comments
import FileHandling
import FollowMoves
import Keyparser
import ParseMoves
import OutputFormatting

main :: IO ()
main = do
  contents <- getPgnContents
  let splitContents = lines contents
  case parseKeys splitContents of
    Nothing -> failed
    Just (keys, body) ->
      case removeComments body of
        Nothing -> failed
        Just cleanedBody ->
          let semiMoves = parseMoves cleanedBody
              moves = followMoves id initialGameState semiMoves
              formattedMoves = formatMoves moves
          in
            writeEgn keys formattedMoves

failed :: IO ()
failed = print "Failed"
