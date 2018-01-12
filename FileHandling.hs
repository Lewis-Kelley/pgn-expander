module FileHandling where

getPgnContents :: IO String
getPgnContents = do
  putStrLn "Enter the name of the file to parse: "
  fileName <- getLine
  readFile fileName

writeEgn :: [String] -> [String] -> IO ()
writeEgn keys moves = do
  putStrLn "Enter the name of the output file: "
  fileName <- getLine
  writeFile fileName ((foldKeys keys) ++ "\n" ++ (foldMoves moves))

foldKeys :: [String] -> String
foldKeys = unlines

foldMoves :: [String] -> String
foldMoves = unlines
