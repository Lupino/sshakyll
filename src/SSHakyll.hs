module SSHakyll
  (
    encodeTreeList,
    treeListToJSON,
    getFileTreeList,
    saveFile,
    deleteFile,
    publish,
    getPublicId
  ) where

import           Control.Monad                  (forM, unless, when)
import           Data.Maybe                     (fromMaybe)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesDirectoryExist,
                                                 doesFileExist,
                                                 getDirectoryContents,
                                                 removeDirectoryRecursive,
                                                 removeFile)
import           System.FilePath                (dropFileName, (</>))

import           System.IO                      (IOMode (ReadMode), hFileSize,
                                                 withFile)

import           Data.Aeson                     (ToJSON (..), Value (..),
                                                 encode, object, (.=))
import qualified Data.ByteString.Lazy           as LB (ByteString, empty,
                                                       writeFile)
import           Data.HashMap.Strict            (union)
import qualified Data.Text                      as T (pack)
import           System.Process                 (callProcess)
import           System.Process.ByteString.Lazy (readProcessWithExitCode)


data FileTree = Directory String [FileTree] | FileName String Int
  deriving (Show)

instance ToJSON FileTree where
  toJSON (Directory dir trees) = object [ T.pack dir .= treeListToJSON trees ]
  toJSON (FileName name size)  = object [T.pack name .= size ]

unionValue :: Value -> Value -> Value
unionValue (Object a) (Object b) = Object $ union a b
unionValue (Object a) _          = Object a
unionValue _ (Object b)          = Object b
unionValue _ _                   = Null

treeListToJSON :: [FileTree] -> Value
treeListToJSON = foldr (unionValue . toJSON) Null

encodeTreeList :: [FileTree] -> LB.ByteString
encodeTreeList = encode . treeListToJSON

getFileTreeList :: FilePath -> IO [FileTree]
getFileTreeList topdir = do
    isDirectory <- doesDirectoryExist topdir
    unless isDirectory $ createDirectoryIfMissing True topdir
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    forM properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory then do
        subTree <- getFileTreeList path
        return $ Directory name subTree
      else do
        size <- fromInteger <$> getFileSize path
        return $ FileName name size

getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize

saveFile :: FilePath -> LB.ByteString -> IO ()
saveFile fn fc = do
  createDirectoryIfMissing True dir
  LB.writeFile fn fc

  where dir = dropFileName fn

deleteFile :: FilePath -> IO ()
deleteFile fn = do
  isDirectory <- doesDirectoryExist fn
  when isDirectory $ removeDirectoryRecursive fn
  fileExists <- doesFileExist fn
  when fileExists $ removeFile fn

publish :: IO ()
publish = callProcess "site" ["rebuild"]

getPublicId :: String -> IO LB.ByteString
getPublicId sessionId = do
  (_, out, _) <- readProcessWithExitCode "getPublicId" [sessionId] LB.empty
  return out
