import Data.Text (unpack)

import Comments
import FileHandling
import FollowMoves
import Keyparser
import ParseMoves
import Types
import Util

main :: IO ()
main = do
  contents <- getPgnContents
  let stringContents = unpack contents
  let splitContents = lines stringContents
  writeGames $ map runGame $ cleanGames $ splitGames splitContents

splitGames :: [String] -> [[String]]
splitGames = splitWhen isScore

isScore :: String -> Bool
isScore "1-0" = True
isScore "0-1" = True
isScore "1/2-1/2" = True
isScore "*" = True
isScore _ = False

cleanGames :: [[String]] -> [[String]]
cleanGames = map $ dropWhile (\ gameLine -> gameLine == "")

runGame :: [String] -> Maybe ([String], [Move])
runGame contents =
  parseKeys contents >>=
  (\ (keys, body) ->
      removeComments body >>=
      (\ cleanedBody ->
         let parsedMoves = parseMoves cleanedBody
         in Just (keys, followMoves id initialGameState parsedMoves)))
