module Comments where

import Data.List
import Util
import Parsing

removeComments :: [String] -> Maybe [String]
removeComments body =
  case removeBlockComments body False id of
    Nothing -> Nothing
    Just cleanedBody -> Just $ removeLineComments cleanedBody

removeBlockComments :: [String] -> Bool -> ([String] -> [String]) -> Maybe [String]
removeBlockComments body wasInComment continuation =
  if null body
  then if wasInComment then Nothing
       else Just (continuation [])
  else let (line, nowInComment) = cleanLine (head body) wasInComment
       in removeBlockComments (tail body)
          nowInComment (attachContinuation continuation line)

cleanLine :: String -> Bool -> (String, Bool)
cleanLine line True = insideBrace line
cleanLine line False = outsideBrace line

insideBrace :: String -> (String, Bool)
insideBrace line =
  let (cleanedLine, foundBrace) = removeUntilClosingBrace line
  in if foundBrace
     then outsideBrace cleanedLine
     else ("", True)

outsideBrace :: String -> (String, Bool)
outsideBrace line =
  case elemIndex '{' line of
    Nothing -> (line, False)
    Just index ->
      let (beforeBrace, afterBrace) = splitAt index line
          (rest, inComment) = insideBrace afterBrace
      in (beforeBrace ++ rest, inComment)

removeUntilClosingBrace :: String -> (String, Bool)
removeUntilClosingBrace str =
  case elemIndex '}' str of
    Nothing -> ("", False)
    Just index ->
      case substring (index + 1) (length str) str of
        Nothing -> ("", True)
        Just shortStr -> (shortStr, True)

removeLineComments :: [String] -> [String]
removeLineComments body = foldr removeLineComment [] body

removeLineComment :: String -> [String] -> [String]
removeLineComment line rest =
  case elemIndex ';' line of
    Nothing -> (line : rest)
    Just index ->
      case substring 0 index line of
        Nothing -> (line : rest)
        Just beforeComment -> (beforeComment : rest)
