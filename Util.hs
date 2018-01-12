module Util where

unMaybeList :: [Maybe a] -> [a]
unMaybeList =
  foldr (\ move rest ->
            case move of
              Nothing -> rest
              Just unwrapped -> (unwrapped : rest)) []

notNothing :: Maybe a -> Bool
notNothing x =
  case x of
    Nothing -> False
    Just _ -> True

attachContinuation :: ([a] -> [a]) -> a -> [a] -> [a]
attachContinuation continuation newElem rest = continuation (newElem : rest)
