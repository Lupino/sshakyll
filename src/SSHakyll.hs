module SSHakyll
  (
    encodeTreeList,
    treeListToJSON,
    getFileTreeList,
    saveFile,
    deleteFile,
    publish
  ) where

import Control.Monad (forM, when)
import System.Directory (doesDirectoryExist, getDirectoryContents,
                         createDirectoryIfMissing, doesFileExist,
                         removeFile, removeDirectory)
import System.FilePath ((</>), dropFileName)
import Data.Maybe (fromMaybe)

import System.IO (withFile, IOMode( ReadMode ), hFileSize)

import Data.Aeson (ToJSON(..), object, (.=), Value(..), encode)
import Data.HashMap.Strict (union)
import qualified Data.ByteString.Lazy as LB (ByteString, writeFile)
import qualified Data.Text as T (pack)

import Site
import Hakyll (Configuration(..), defaultConfiguration)


data FileTree = Directory String [FileTree] | FileName String Int
  deriving (Show)

instance ToJSON FileTree where
  toJSON (Directory dir trees) = object [ T.pack dir .= treeListToJSON trees ]
  toJSON (FileName name size)  = object [T.pack name .= size ]

unionValue :: Value -> Value -> Value
unionValue (Object a) (Object b) = Object $ union a b
unionValue (Object a) _ = Object a
unionValue _ (Object b) = Object b
unionValue _ _ = Null

treeListToJSON :: [FileTree] -> Value
treeListToJSON = foldr (unionValue . toJSON) Null

encodeTreeList :: [FileTree] -> LB.ByteString
encodeTreeList = encode . treeListToJSON

getFileTreeList :: FilePath -> IO [FileTree]
getFileTreeList topdir = do
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
  when isDirectory $ removeDirectory fn
  fileExists <- doesFileExist fn
  when fileExists $ removeFile fn

publish :: FilePath -> IO ()
publish root = do
  buildWithExitCode conf path
  return ()

  where conf = defaultConfiguration { destinationDirectory = root </> "www",
                                      storeDirectory = root </> "_cache",
                                      tmpDirectory = root </> "_cache/tmp",
                                      providerDirectory = root </> "source" }
        path = root </> "source" </> "config.json"
