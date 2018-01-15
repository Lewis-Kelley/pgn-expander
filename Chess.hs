import Comments
import FileHandling
import FollowMoves
import Keyparser
import ParseMoves
import Util

main :: IO ()
main = do
  contents <- getPgnContents
  let splitContents = lines contents
  writeGames $ map runGame $ splitGames splitContents

splitGames :: [String] -> [[String]]
splitGames = splitWhen isScore

isScore :: String -> Bool
isScore "1-0" = True
isScore "0-1" = True
isScore "1/2-1/2" = True
isScore "*" = True
isScore _ = False

runGame :: [String] -> Maybe ([String], [Move])
runGame contents =
  parseKeys contents >>=
  (\ (keys, body) ->
      removeComments body >>=
      (\ cleanedBody ->
         let parsedMoves = parseMoves cleanedBody
         in Just (keys, followMoves id initialGameState parsedMoves)))

