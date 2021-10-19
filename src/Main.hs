
main = do
  fName <- readLn
  contents <- readFile fName
  putStrLn contents