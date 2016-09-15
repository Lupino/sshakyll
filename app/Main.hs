{-# LANGUAGE OverloadedStrings #-}
module Main where

import SSHakyll
import Network (PortID(PortNumber))
import Web.Scotty
import Network.HTTP.Types
import Network.Wai (Request(..))
import Network.Wai.Handler.Warp (setPort, setHost)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)
import Data.Default.Class (def)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>), dropDrive)
import qualified Data.Text as T (pack, unpack)
import Data.Aeson (object, (.=))

main :: IO ()
main = do
  let opts = def { settings = setPort 10000 $ setHost (Host "127.0.0.1") (settings def) }
  scottyOpts opts $ do
    middleware logStdoutDev
    middleware staticMid

    get "/api/file" $ do
      trees <- liftIO $ getFileTreeList root
      json $ treeListToJSON trees

    put (regex "^/api/file/(.*)") $ do
      path <- filePath root
      wb <- body
      liftIO $ saveFile path wb

      json $ object [ "result" .= T.pack "OK" ]

    delete (regex "^/api/file/(.*)") $ do
      path <- filePath root
      liftIO $ deleteFile path
      json $ object [ "result" .= T.pack "OK" ]

    post "/api/publish" $ do
      code <- liftIO $ publish root
      json $ object [ "result" .= code ]

  where staticMid = staticPolicy (addBase "public")
        root = "site"

filePath :: FilePath -> ActionM FilePath
filePath root = do
  req <- request
  let path = foldr ((</>) . T.unpack) "" (drop 2 $ pathInfo req)
  return $ root </> "source" </> path
