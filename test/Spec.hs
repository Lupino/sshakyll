import SSHakyll

main :: IO ()
main = do
  trees <- getFileTreeList "."
  print $ encodeTreeList trees
