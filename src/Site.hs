{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Site
  (
    buildWithExitCode
  ) where

import Data.Monoid (mappend)
import Hakyll
import Hakyll.Commands (rebuild)
import qualified Hakyll.Core.Logger as Logger
import Data.String (fromString)
import qualified Data.Text as T (unpack, Text)
import Data.Aeson (FromJSON(..), (.:), withObject, decodeStrict, Value(..), Array)
import System.IO (readFile)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.ByteString.Char8 (pack)
import System.Exit (ExitCode(ExitSuccess))
import Data.Char (ord)
import Numeric (showHex)
import Data.Vector (toList)

data Page = Page { getPagePattern   :: Value,
                   getPageTemplates :: [FilePath] }

instance FromJSON Page where
  parseJSON = withObject "Page" $ \o -> do
    getPagePattern   <- o .: "pattern"
    getPageTemplates <- o .: "templates"
    return Page{..}

data Field = Field { getFieldPattern :: String,
                     getFieldName    :: String }

instance FromJSON Field where
  parseJSON = withObject "Field" $ \o -> do
    getFieldPattern <- o .: "pattern"
    getFieldName    <- o .: "field"
    return Field{..}

data Archive = Archive { getArchiveFile      :: FilePath,
                         getArchiveExists    :: Bool,
                         getArchiveTitle     :: String,
                         getArchiveFieldList :: [Field],
                         getArchiveTemplates :: [FilePath] }

instance FromJSON Archive where
  parseJSON = withObject "Archive" $ \o -> do
    getArchiveFile      <- o .: "file"
    getArchiveTitle     <- o .: "title"
    getArchiveFieldList <- o .: "fields"
    getArchiveTemplates <- o .: "templates"
    let getArchiveExists = False
    return Archive{..}

data Config = Config { getPageList    :: [Page],
                       getArchiveList :: [Archive] }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    getPageList    <- o .: "pages"
    getArchiveList <- o .: "archives"
    return Config{..}

valueToPattern :: Value -> Pattern
valueToPattern (String v) = fromString $ T.unpack v
valueToPattern (Array  v) = fromList $ extractValue v
valueToPattern _          = error "Pattern"

extractValue :: Array -> [Identifier]
extractValue v = map extract $ toList v
  where extract (String v) = fromString $ T.unpack v
        extract _ = ""

pageToRules :: Page -> Rules ()
pageToRules page =  match pat $ do
  route $ setExtension "html"
  compile $ pandocCompiler
      >>= loadAndApplyTemplateList tpls postCtx
      >>= relativizeUrls

  where pat   = valueToPattern $ getPagePattern page
        tpls  = map fromString $ getPageTemplates page

archiveToRules :: Archive -> Rules ()
archiveToRules archive = rule $ do
  route idRoute
  compile $ do
    fc <- fieldListToContext fields

    let archiveCtx = constField "title" title `mappend` fc

    let body = if exists then getResourceBody >>= applyAsTemplate archiveCtx
               else makeItem ""

    body
        >>= loadAndApplyTemplateList tpls archiveCtx
        >>= relativizeUrls

  where file   = getArchiveFile archive
        tpls   = map fromString $ getArchiveTemplates archive
        title  = getArchiveTitle archive
        fields = getArchiveFieldList archive
        exists = getArchiveExists archive
        rule   = if exists then match (fromString file) else create [fromString file]

configToRules :: Config -> Rules ()
configToRules config = do
  mapM_ pageToRules pages
  mapM_ archiveToRules archives

  where pages = getPageList config
        archives = getArchiveList config

loadAndApplyTemplateList :: [Identifier]            -- ^ Template identifier
                         -> Context String          -- ^ Context
                         -> Item String             -- ^ Page
                         -> Compiler (Item String)  -- ^ Resulting item

loadAndApplyTemplateList (x:xs) ctx item = do
  newItem <- loadAndApplyTemplate x ctx item
  loadAndApplyTemplateList xs ctx newItem
loadAndApplyTemplateList [] _ item = return item

fieldToContext :: Field -> Compiler (Context String)
fieldToContext field = do
  posts <- recentFirst =<< loadAll pat
  return $ listField name postCtx (return posts)
  where pat = fromString $ getFieldPattern field
        name  = getFieldName field

fieldListToContext :: [Field] -> Compiler (Context String)
fieldListToContext (x:xs) = do
  x' <- fieldToContext x
  xs' <- fieldListToContext xs
  return $ x' `mappend` xs'

fieldListToContext [] = return defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

fillArchive :: FilePath -> Archive -> IO Archive
fillArchive root archive = update <$> doesFileExist fn
  where fn = root </> getArchiveFile archive
        update exists = archive { getArchiveExists = exists }

preParseUnicode :: String -> String
preParseUnicode (x:xs) = if ord x > 255 then "\\u" ++ showHex (ord x) "" ++ preParseUnicode xs
                         else x:preParseUnicode xs
preParseUnicode [] = []

readConfig :: FilePath -> IO (Maybe Config)
readConfig fn = parseConfig . preParseUnicode <$> readFile fn
  where parseConfig str = decodeStrict $ pack str

buildWithExitCode :: Configuration -> FilePath -> IO ExitCode
buildWithExitCode conf fn = do
  (Just config) <- readConfig fn
  archives <- mapM (fillArchive root) $ getArchiveList config
  logger <- Logger.new Logger.Error
  rebuild  conf logger $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    configToRules config { getArchiveList = archives }

    match "templates/*" $ compile templateBodyCompiler

  where root = providerDirectory conf
