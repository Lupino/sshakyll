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
import Data.Aeson (FromJSON(..), (.:), withObject, decodeStrict)
import System.IO (readFile)
import Data.ByteString.Char8 (pack)
import System.Exit (ExitCode(ExitSuccess))

data Post = Post { getPostRegex     :: String,
                   getPostTemplates :: [FilePath] }

instance FromJSON Post where
  parseJSON = withObject "Post" $ \o -> do
    getPostRegex     <- o .: "regex"
    getPostTemplates <- o .: "templates"
    return Post{..}

data Page = Page { getPageFileList  :: [FilePath],
                   getPageTemplates :: [FilePath] }

instance FromJSON Page where
  parseJSON = withObject "Page" $ \o -> do
    getPageFileList  <- o .: "files"
    getPageTemplates <- o .: "templates"
    return Page{..}

data Field = Field { getFieldRegex :: String,
                     getFieldName  :: String }

instance FromJSON Field where
  parseJSON = withObject "Field" $ \o -> do
    getFieldRegex <- o .: "regex"
    getFieldName  <- o .: "field"
    return Field{..}

data Archive = Archive { getArchiveFile      :: FilePath,
                         getArchiveTitle     :: String,
                         getArchiveFieldList :: [Field],
                         getArchiveTemplates :: [FilePath] }

instance FromJSON Archive where
  parseJSON = withObject "Archive" $ \o -> do
    getArchiveFile      <- o .: "file"
    getArchiveTitle     <- o .: "title"
    getArchiveFieldList <- o .: "fields"
    getArchiveTemplates <- o .: "templates"
    return Archive{..}

data Config = Config { getPostList    :: [Post],
                       getPageList    :: [Page],
                       getArchiveList :: [Archive] }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    getPostList    <- o .: "posts"
    getPageList    <- o .: "pages"
    getArchiveList <- o .: "archives"
    return Config{..}

postToRules :: Post -> Rules ()
postToRules post = match regex $ do
  route $ setExtension "html"
  compile $ pandocCompiler
      >>= loadAndApplyTemplateList tpls postCtx
      >>= relativizeUrls
  where regex = fromString $ getPostRegex post
        tpls  = map fromString $ getPostTemplates post

pageToRules :: Page -> Rules ()
pageToRules page =  match (fromList files) $ do
  route $ setExtension "html"
  compile $ pandocCompiler
      >>= loadAndApplyTemplateList tpls postCtx
      >>= relativizeUrls

  where files = map fromString $ getPageFileList page
        tpls  = map fromString $ getPageTemplates page

archiveToRules :: Archive -> Rules ()
archiveToRules archive = match file $ do
  route idRoute
  compile $ do
    fc <- fieldListToContext fields

    let archiveCtx = constField "title" title `mappend` fc

    getResourceBody
        >>= applyAsTemplate archiveCtx
        >>= loadAndApplyTemplateList tpls archiveCtx
        >>= relativizeUrls

  where file   = fromString $ getArchiveFile archive
        tpls   = map fromString $ getArchiveTemplates archive
        title  = getArchiveTitle archive
        fields = getArchiveFieldList archive

configToRules :: Config -> Rules ()
configToRules config = do
  mapM_ postToRules posts
  mapM_ pageToRules pages
  mapM_ archiveToRules archives

  where posts = getPostList config
        pages = getPageList config
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
  posts <- recentFirst =<< loadAll regex
  return $ listField name postCtx (return posts)
  where regex = fromString $ getFieldRegex field
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

readConfig :: FilePath -> IO (Maybe Config)
readConfig fn = parseConfig <$> readFile fn
  where parseConfig str = decodeStrict $ pack str

buildWithExitCode :: Configuration -> FilePath -> IO ExitCode
buildWithExitCode conf fn = do
  (Just config) <- readConfig fn
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

    configToRules config

    match "templates/*" $ compile templateBodyCompiler
