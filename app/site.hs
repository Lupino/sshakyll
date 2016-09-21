{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Data.Monoid (mappend)
import Hakyll
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

data Static = Static { getStaticPattern :: Value,
                       getStaticType    :: String }

instance FromJSON Static where
  parseJSON = withObject "Static" $ \o -> do
    getStaticPattern <- o .: "pattern"
    getStaticType    <- o .: "type"
    return Static{..}

data Config = Config { getPageList    :: [Page],
                       getArchiveList :: [Archive],
                       getStaticList  :: [Static] }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    getPageList    <- o .: "pages"
    getArchiveList <- o .: "archives"
    getStaticList  <- o .: "statics"
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

staticToRules :: Static -> Rules ()
staticToRules static = match pat $ do
  route   idRoute
  compiler t
  where pat = valueToPattern $ getStaticPattern static
        t = getStaticType static
        compiler t | t == "css"      = compile compressCssCompiler
                   | t == "template" = compile templateBodyCompiler
                   | otherwise       = compile copyFileCompiler

configToRules :: Config -> Rules ()
configToRules config = do
  mapM_ pageToRules pages
  mapM_ archiveToRules archives
  mapM_ staticToRules statics

  where pages    = getPageList config
        archives = getArchiveList config
        statics  = getStaticList config

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

isRoot :: FilePath -> IO Bool
isRoot root = doesFileExist fn
  where fn = root </> "source" </> "config.json"

guessRoot :: [FilePath] -> IO FilePath
guessRoot (x:xs) = do
  isr <- isRoot x
  if isr then return x
  else guessRoot xs

guessRoot [] = error "site not found."

main :: IO ()
main = do
  root <- guessRoot [".", "site", "var", "/var", "/var/site"]
  let
      source = root </> "source"
      conf = defaultConfiguration { destinationDirectory = root </> "www",
                                    storeDirectory = root </> "_cache",
                                    tmpDirectory = root </> "_cache/tmp",
                                    providerDirectory = root </> "source" }
      path = root </> "source" </> "config.json"

  (Just config) <- readConfig path
  archives <- mapM (fillArchive source) $ getArchiveList config
  hakyllWith conf $ configToRules config { getArchiveList = archives }
