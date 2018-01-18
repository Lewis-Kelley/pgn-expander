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
  let gameIndices = 1 : map (+1) gameIndices
  let gameIds = map GameID gameIndices
  writeGames $ zipWith runGame gameIds $ cleanGames $ splitGames splitContents

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

runGame :: GameID -> [String] -> Maybe ([String], [Move])
runGame gameId contents =
  parseKeys contents >>=
  (\ (keys, body) ->
      removeComments body >>=
      (\ cleanedBody ->
         let parsedMoves = parseMoves cleanedBody
         in followMoves id gameId initialGameState parsedMoves >>=
            (\ moves -> Just (keys, moves))))
