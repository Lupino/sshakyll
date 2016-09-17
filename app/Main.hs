{-# LANGUAGE OverloadedStrings #-}
module Main where

import SSHakyll
import Network (PortID(PortNumber))
import Web.Scotty (get, post, delete, put, raw, settings, request, json, regex, header,
                   ActionM, redirect, setHeader, scottyOpts, body, middleware)
import Network.Wai (Request(..))
import Network.Wai.Handler.Warp (setPort, setHost)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)
import Data.Default.Class (def)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>), dropDrive)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Lazy as TL (pack, unpack)
import qualified Data.ByteString.Char8 as BC (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL (readFile)
import Data.Aeson (object, (.=))
import Network.Mime (MimeType, defaultMimeLookup)
import Data.Maybe (fromJust)

import Options.Applicative (Parser(..), execParser, strOption, option, auto,
                            long, short, help, value, (<*>), (<>), helper,
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
                                <> value "site" )

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

    get "/" $ redirect "/index.html"

    get "/api/file" $ do
      trees <- liftIO $ getFileTreeList $ root </> "source"
      json $ treeListToJSON trees

    get (regex "^/api/file/(.*)") $ do
      path <- filePath root
      let headers = [("Content-Type", getMimeType path)]
      setHeader "Content-Type" $ TL.pack $ BC.unpack $ getMimeType path
      fc <- liftIO $ BL.readFile path
      raw fc

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

    get "/api/publicId" $ do
      sessionId <- TL.unpack . fromJust <$> header "X-Sandstorm-Session-Id"
      fc <- liftIO $ getPublicId sessionId
      raw fc


  where staticMid = staticPolicy (addBase "public")
        staticMid' = staticPolicy (addBase $ root </> "source")
        port = getPort opts
        host = getHost opts
        root = getRoot opts
        serverOpts = def { settings = setPort port $ setHost (Host host) (settings def) }

filePath :: FilePath -> ActionM FilePath
filePath root = do
  req <- request
  let path = foldr ((</>) . T.unpack) "" (drop 2 $ pathInfo req)
  return $ root </> "source" </> path


-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack
