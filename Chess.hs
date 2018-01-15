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
  case runGame splitContents of
    Nothing -> print "Failed"
    Just (keys, moves) -> writeEgn keys $ formatMoves moves

runGame :: [String] -> Maybe ([String], [Move])
runGame contents =
  parseKeys contents >>=
  (\ (keys, body) ->
      removeComments body >>=
      (\ cleanedBody ->
         let parsedMoves = parseMoves cleanedBody
         in Just (keys, followMoves id initialGameState parsedMoves)))

