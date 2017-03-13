{-# LANGUAGE OverloadedStrings #-}
module Main where

import SSHakyll
import Network (PortID(PortNumber))
import Web.Scotty (get, post, delete, put, raw, settings, param, json, header,
                   ActionM, redirect, setHeader, scottyOpts, body, middleware,
                   status, text, RoutePattern, function)
import Network.Wai (Request(..))
import Network.Wai.Handler.Warp (setPort, setHost)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)
import Network.HTTP.Types (status404)
import Data.Default.Class (def)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>), dropDrive, dropFileName)
import Data.List (isPrefixOf)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Lazy as TL (pack, unpack)
import qualified Data.ByteString.Char8 as BC (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (readFile)
import Data.Aeson (object, (.=))
import Network.Mime (MimeType, defaultMimeLookup)
import Data.Maybe (fromJust)
import Codec.Archive.Zip (toArchive, ZipOption (..), extractFilesFromArchive)
import System.Directory (doesFileExist)

import Data.Semigroup ((<>))
import Options.Applicative (Parser(..), execParser, strOption, option, auto,
                            long, short, help, value, (<*>), helper,
                            fullDesc, info, progDesc, metavar)

data Options = Options { getHost :: String,
                         getPort :: Int,
                         getRoot :: String }

parser :: Parser Options
parser = Options <$> strOption (long "host"
                                <> short 'h'
                                <> metavar "HOST"
                                <> help "The sshakyll server host."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                  <> short 'p'
                                  <> metavar "PORT"
                                  <> help "The sshakyll server port."
                                  <> value 8000 )
                 <*> strOption (long "directory"
                                <> short 'd'
                                <> metavar "DIR"
                                <> help "Site root dirctory."
                                <> value "" )

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      (fullDesc
       <> progDesc "SSHakyll server" )

program :: Options -> IO ()
program opts =
  scottyOpts serverOpts $ do
    middleware logStdout
    middleware staticMid
    middleware staticMid'

    get "/" $ do
      hasEditor <- liftIO $ doesFileExist editor
      if hasEditor then redirect "/editor/index.html"
      else redirect "/index.html"

    get "/api/file" $ do
      trees <- liftIO $ getFileTreeList $ root </> "source"
      json $ treeListToJSON trees

    get (textRoute [ "api", "file" ]) $ do
      path <- filePath root
      let headers = [("Content-Type", getMimeType path)]
      setHeader "Content-Type" $ TL.pack $ BC.unpack $ getMimeType path
      fileExists <- liftIO $ doesFileExist path
      if fileExists then do
        fc <- liftIO $ BL.readFile path
        raw fc
      else do
        status status404
        text ""

    put (textRoute [ "api", "file" ]) $ do
      path <- filePath root
      wb <- body
      liftIO $ saveFile path wb

      json $ object [ "result" .= T.pack "OK" ]

    put (textRoute [ "api", "archive" ]) $ do
      path <- filePath root
      wb <- body
      liftIO $ extractFilesFromArchive [OptDestination (dropFileName path)] $ toArchive wb

      json $ object [ "result" .= T.pack "OK" ]

    delete (textRoute [ "api", "file" ]) $ do
      path <- filePath root
      liftIO $ deleteFile path
      json $ object [ "result" .= T.pack "OK" ]

    post "/api/publish" $ do
      fc <- liftIO publish
      raw fc

    get "/api/publicId" $ do
      sessionId <- TL.unpack . fromJust <$> header "X-Sandstorm-Session-Id"
      fc <- liftIO $ getPublicId sessionId
      raw fc


  where staticMid = staticPolicy (addBase "public")
        staticMid' = staticPolicy (addBase $ root </> "source")
        port = getPort opts
        host = getHost opts
        root = getRoot opts
        editor = root </> "source" </> "editor" </> "index.html"
        serverOpts = def { settings = setPort port $ setHost (Host host) (settings def) }

filePath :: FilePath -> ActionM FilePath
filePath root = do
  path <- param "path"
  return $ root </> "source" </> path

textRoute :: [T.Text] -> RoutePattern
textRoute strs = function $ \req ->
  if isPrefixOf strs (pathInfo req) then
    Just [("path", TL.pack $ foldr ((</>) . T.unpack) "" (drop 2 $ pathInfo req))]
  else Nothing


-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack
