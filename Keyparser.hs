module Keyparser where

import Data.Map.Strict

import Parsing
import Types

parseKeys :: [String] -> Maybe (KeyValMap, [String])
parseKeys contents = do
  (keyVals, rest) <- rawParseKeys contents
  Just ((KeyValMap $ fromList keyVals), rest)

rawParseKeys :: [String] -> Maybe ([(String, String)], [String])
rawParseKeys [] = Just ([], [])
rawParseKeys pgn =
  if head pgn == ""
  then Just ([], tail pgn)
  else
    do
      keyVal <- parseKeyVal $ head pgn
      (keyVals, body) <- rawParseKeys $ tail pgn
      Just ((keyVal : keyVals), body)

parseKeyVal :: String -> Maybe (String, String)
parseKeyVal keyLine = do
  cleanKeyLine <- insideBrackets keyLine
  key <- getKey cleanKeyLine
  value <- insideQuotes cleanKeyLine
  Just (key, value)

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
