module Keyparser where

import Parsing

parseKeys :: [String] -> Maybe ([String], [String])
parseKeys pgn =
  if head pgn == "" then Just ([], tail pgn)
  else case parseKey $ head pgn of
    Nothing -> Nothing
    Just key ->
      case parseKeys $ tail pgn of
        Nothing -> Nothing
        Just (keys, body) -> Just ((key : keys), body)

parseKey :: String -> Maybe String
parseKey keyLine =
  case insideBrackets keyLine of
    Nothing -> Nothing
    Just cleanKeyLine ->
      let key = getKey cleanKeyLine
          value = insideQuotes cleanKeyLine
      in getKeyValue key value

insideBrackets :: String -> Maybe String
insideBrackets = insideChars '[' ']'

insideQuotes :: String -> Maybe String
insideQuotes = insideChars '"' '"'

getKey :: String -> Maybe String
getKey keyLine =
  case words keyLine of
    [] -> Nothing
    (key : _) -> Just key

getKeyValue :: Maybe String -> Maybe String -> Maybe String
getKeyValue mKey mValue =
  case mKey of
    Nothing -> Nothing
    Just key ->
      case mValue of
        Nothing -> Nothing
        Just value ->
          Just (key ++ ";" ++ value)
