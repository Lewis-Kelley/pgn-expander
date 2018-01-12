module Parsing where

import Data.List

insideChars :: Char -> Char -> String -> Maybe String
insideChars leftMarker rightMarker str
  | leftMarker == rightMarker =
    let indices = elemIndices leftMarker str in
      if length indices /= 2
      then Nothing
      else substring ((indices !! 0) + 1) (indices !! 1) str
  | otherwise =
    let leftIndices = elemIndices leftMarker str
        rightIndices = elemIndices rightMarker str in
      if length leftIndices /= 1 || length rightIndices /= 1
      then Nothing
      else substring ((leftIndices !! 0) + 1) (rightIndices !! 0) str

substring :: Int -> Int -> String -> Maybe String
substring leftIndex rightIndex str =
  if leftIndex > rightIndex then Nothing
  else let (_, rightSection) = splitAt leftIndex str
           (midSection, _) = splitAt (rightIndex - leftIndex) rightSection
       in Just midSection
