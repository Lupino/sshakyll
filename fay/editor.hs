{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Main (main) where

import Prelude
import FFI (ffi, Defined(..))
import DOM hiding (RequestMethod(..), open)
import Data.Text (fromString, Text)
import qualified Data.Text as T

type FilePath = Text

appendPath :: FilePath -> FilePath -> FilePath
appendPath = ffi "(function(f, g) { p = f + '/' + g; return p.replace(/\\/+/g, '/') })(%1, %2)"

(</>) :: FilePath -> FilePath -> FilePath
f </> g = appendPath f g
infixr 5 </>

dropFileName :: FilePath -> FilePath
dropFileName = ffi "(function(fn) { return fn.substr(0, fn.lastIndexOf('/')) })(%1)"

data FetchOptions = FetchOptions { getUrl     :: T.Text,
                                   getMethod  :: RequestMethod,
                                   getData    :: Defined T.Text,
                                   getResolve :: XMLHttpRequest -> Fay (),
                                   getReject  :: T.Text -> Fay () }


fetchOptions :: FetchOptions
fetchOptions = FetchOptions { getUrl     = "",
                              getMethod  = GET,
                              getData    = Undefined,
                              getResolve = const $ return (),
                              getReject  = const $ return () }

data RequestMethod = GET | POST | PUT | DELETE

open :: RequestMethod -> Text -> XMLHttpRequest -> Fay XMLHttpRequest
open = ffi "(function(method, url, xhr) { xhr['open'](method['instance'], url, true); return xhr; })(%1, %2, %3)"

send_ :: T.Text -> XMLHttpRequest -> Fay ()
send_ = ffi "%2['send'](%1)"

setOnLoadHandler :: (XMLHttpRequest -> Fay ()) -> XMLHttpRequest -> Fay XMLHttpRequest
setOnLoadHandler = ffi "(function(handler, xhr) { xhr['onload'] = function() { handler(xhr); }; return xhr; })(%1, %2)"

setOnErrorHandler :: (T.Text -> Fay ()) -> XMLHttpRequest -> Fay XMLHttpRequest
setOnErrorHandler = ffi "(function(handler, xhr) { xhr['onerror'] = function(e) { handler(e.toString()); }; return xhr; })(%1, %2)"

fetch_ :: FetchOptions -> Fay ()
fetch_ opts = xmlHttpRequest
  >>= open method url
  >>= setOnLoadHandler resolve
  >>= setOnErrorHandler reject
  >>= sendData dat

  where method = getMethod opts
        url = getUrl opts
        dat = getData opts
        resolve = getResolve opts
        reject  = getReject opts
        sendData :: Defined T.Text -> XMLHttpRequest -> Fay ()
        sendData (Defined dt) = send_ dt
        sendData Undefined = send


fetch :: T.Text -> FetchOptions -> (XMLHttpRequest -> Fay ()) -> Fay ()
fetch url opts cb = fetch_ $ opts { getUrl = url, getResolve = cb }

get :: T.Text -> (XMLHttpRequest -> Fay ()) -> Fay ()
get ur = fetch ur fetchOptions

post :: T.Text -> T.Text -> (XMLHttpRequest -> Fay ()) -> Fay ()
post ur dat = fetch ur opts
  where opts = fetchOptions { getData = Defined dat, getMethod = POST }

put :: T.Text -> T.Text -> (XMLHttpRequest -> Fay ()) -> Fay ()
put ur dat = fetch ur opts
  where opts = fetchOptions { getData = Defined dat, getMethod = PUT }

delete :: T.Text -> (XMLHttpRequest -> Fay ()) -> Fay ()
delete ur = fetch ur fetchOptions { getMethod = DELETE }

-- getAction error body
data ActionT = ActionT { getAction :: Maybe T.Text -> Maybe T.Text -> Fay () }

fromAction :: ActionT -> XMLHttpRequest -> Fay ()
fromAction act = handler
  where act' = getAction act
        handler :: XMLHttpRequest -> Fay ()
        handler xhr = do
          st <- status xhr
          rt <- responseText xhr
          if st >= 400 then
            act' (Just (T.concat ["XHR returned status ", T.pack (show st), ":\n", rt])) Nothing
          else
            act' Nothing (Just rt)

toAction :: (Maybe T.Text -> Maybe T.Text -> Fay ()) -> ActionT
toAction act = ActionT { getAction = act }

saveFile :: FilePath -> T.Text -> ActionT -> Fay ()
saveFile fn body act = put url body handler
  where handler = fromAction act
        url = "/api/file" </> fn

readFile :: FilePath -> ActionT -> Fay ()
readFile fn act = get url handler
  where handler = fromAction act
        url = "/api/file" </> fn

deleteFile :: FilePath -> ActionT -> Fay ()
deleteFile fn act = delete ur handler
  where handler = fromAction act
        ur = "/api/file" </> fn

data SaveState = Saved | Saving | Unsave

setSaveState :: SaveState -> Fay ()
setSaveState = ffi "(function (state) { window.saveState = state['instance']})(%1)"

getSaveState :: Fay SaveState
getSaveState = ffi "{ instance: window.saveState }"

setTimer :: Timer -> Fay ()
setTimer = ffi "(function (t) { window.saveTimeout = t; })(%1)"

getTimer :: Fay Timer
getTimer = ffi "window.saveTimeout"

saveBtn :: Fay Element
saveBtn = getElementById "save"

setProp :: Text -> Text -> Element -> Fay Element
setProp = ffi "(function(prop, val, elem) { elem[prop] = val; return elem })(%1, %2, %3)"

removeProp :: Text -> Element -> Fay Element
removeProp = ffi "(function(prop, elem) { elem[prop] = null; return elem })(%1, %2)"

saved :: Fay ()
saved = do
  st <- getSaveState
  case st of
    Saving -> do
      saveBtn >>= setHtml "Saved"
      setSaveState Saved
    _ -> return ()

saving :: Fay ()
saving = do
  saveBtn >>= setProp "disabled" "disabled" >>= setHtml "Saving..."
  setSaveState Saving

unsaved :: Fay ()
unsaved = do
  setSaveState Unsave
  saveBtn >>= removeProp "disabled" >>= setHtml "Save"
  t <- getTimer
  clearTimeout t
  setTimeout 1000 save
  return ()
  where save :: Timer -> Fay ()
        save t = do
          setTimer t
          saveCurrent

getCurrentPath :: Fay FilePath
getCurrentPath = ffi "window.currentPath"

setCurrentPath :: FilePath -> Fay ()
setCurrentPath = ffi "(function(p){window.currentPath = p })(%1)"

getCurrentDirectory :: Fay FilePath
getCurrentDirectory = ffi "window.currentDirectory"

setCurrentDirectory :: FilePath -> Fay ()
setCurrentDirectory = ffi "(function(p){window.currentDirectory = p })(%1)"

saveErrorElem :: Fay Element
saveErrorElem = getElementById "save-error"

isUnsave :: SaveState -> Bool
isUnsave Unsave = True
isUnsave _ = False

isTextFile :: FilePath -> Bool
isTextFile = ffi "isTextFile(%1)"

isImageFile :: FilePath -> Bool
isImageFile = ffi "isImageFile(%1)"

saveCurrent :: Fay ()
saveCurrent = do
  saveErrorElem >>= setHtml ""
  currentPath <- getCurrentPath
  saveState <- getSaveState

  when (Prelude.not (T.null currentPath) && isUnsave saveState && isTextFile currentPath) $ do
    saving
    editor <- getEditor
    dat <- getEditorValue editor
    saveFile currentPath dat $ toAction act

  where act :: Maybe Text -> Maybe Text -> Fay ()
        act (Just _) Nothing = unsaved
        act Nothing (Just _) = saved

prompt :: Text -> Fay Text
prompt = ffi "window.prompt(%1)"

newDoc :: Event -> Fay ()
newDoc _ = do
  saveCurrent
  fn <- prompt "Enter file name."
  currentDirectory <- getCurrentDirectory

  let path = currentDirectory </> fn

  put ("/api/file" </> fixed path (isTextFile fn)) "\n" (const updateTree)

  where fixed fn True  = fn
        fixed fn False = fn T.<> ".md"

deleteDoc :: Event -> Fay ()
deleteDoc _ = do
  currentPath <- getCurrentPath
  unless (T.null currentPath) $ do
    c <- confirm ("Delete " T.<> currentPath T.<> " ?")
    when c $ deleteFile currentPath $ toAction resolve
  where resolve :: Maybe Text -> Maybe Text -> Fay ()
        resolve (Just err) Nothing = T.putStrLn err
        resolve Nothing (Just _) = updateTree

callback :: XMLHttpRequest -> Fay ()
callback xhr = responseText xhr >>= T.putStrLn

data Article = Article { getTitle :: T.Text, getContent :: T.Text }

trim :: T.Text -> T.Text
trim = ffi "%1.trim()"

parseArticle :: T.Text -> Article
parseArticle txt = parse $ T.lines txt
  where parse :: [T.Text] -> Article
        parse xs | hasTagLine xs = parse_ xs
                 | otherwise     = Article { getTitle = "", getContent = T.concat xs }

        hasTagLine :: [T.Text] -> Bool
        hasTagLine = foldr ((||) . isTagLine) False

        isTagLine :: T.Text -> Bool
        isTagLine x = trim x == "---"

        isTitleLine :: T.Text -> Bool
        isTitleLine x = "title" `T.isPrefixOf` x'
          where x' = trim x

        parse_ :: [T.Text] -> Article
        parse_ (x:xs) = if isTitleLine x then
                          Article { getTitle = trim $ getT x, getContent = getC xs }
                        else parse_ xs
        parse_ []     = Article { getTitle = "", getContent = "" }

        getT :: T.Text -> T.Text
        getT = last . T.splitOn ':'
        getC :: [T.Text] -> T.Text
        getC (x:xs) = if isTagLine x then T.unlines xs
                      else getC xs

renderMarkdown :: T.Text -> T.Text
renderMarkdown = ffi "window.md.render(%1)"

confirm :: T.Text -> Fay Bool
confirm = ffi "window.confirm(%1)"

addEventListener :: T.Text -> (Event -> Fay a) -> Element ->  Fay ()
addEventListener = ffi "(function(evt, func, elem) {elem.addEventListener(evt, func)})(%1, %2, %3)"

publishEvent :: Event -> Fay ()
publishEvent _ = do
  c <- confirm "Publish Now"
  when c $ post "/api/publish" "" callback

data TreeNode = TreeNode { isDir :: Bool, serverPath :: T.Text, text :: T.Text }

initTree :: Text -> (TreeNode -> Fay ()) -> Fay ()
initTree = ffi "initTree(%1, %2)"

clearTree :: Fay()
clearTree = ffi "clearTree()"

updateTree :: Fay ()
updateTree = do
  clearTree
  loadTree treeNodeAction

loadTree :: (TreeNode -> Fay ()) -> Fay ()
loadTree act = get "/api/file" resolve
  where resolve :: XMLHttpRequest -> Fay ()
        resolve xhr = do
          t <- responseText xhr
          initTree t act

setOutput :: T.Text -> Fay ()
setOutput = ffi "setOutput(%1)"

data Editor

getEditor :: Fay Editor
getEditor = initEditor

initEditor :: Fay Editor
initEditor = ffi "initEditor()"

setEditorValue :: Text -> Editor -> Fay Editor
setEditorValue = ffi "(function(value, editor) { editor.setValue(value, -1); return editor })(%1, %2)"

getEditorValue :: Editor -> Fay Text
getEditorValue = ffi "(function(editor) { return editor.getValue() })(%1)"

setEditorMode :: FilePath -> Editor -> Fay Editor
setEditorMode = ffi "setEditorMode(%2, %1)"

setEditorEvent :: Text -> (Event -> Fay ()) -> Editor -> Fay Editor
setEditorEvent = ffi "(function (evt, func, editor) { editor.on(evt, func); return editor; })(%1, %2, %3)"

removeAllEditorEvent :: Text -> Editor -> Fay Editor
removeAllEditorEvent = ffi "(function(evt, editor) { editor.removeAllListeners(evt); return editor; })(%1, %2)"

resize :: Editor -> Fay Editor
resize = ffi "(function(editor) {editor.resize(); return editor})(%1)"

readFileAction :: FilePath -> Maybe Text -> Maybe Text -> Fay ()
readFileAction _ (Just err) Nothing = error $ T.unpack err
readFileAction fn Nothing (Just body) = do
  getEditor
           >>= removeAllEditorEvent "change"
           >>= setEditorValue body
           >>= setEditorEvent "change" (const unsaved)
           >>= setEditorEvent "change" (const $ updatePreview fn)
           >>= setEditorMode fn
  setTimeout 100 $ const (updatePreview fn)
  return ()

setHtml :: Text -> Element -> Fay Element
setHtml = ffi "(function(text, elem) { elem.innerHTML = text; return elem; })(%1, %2)"

showCurrentPath :: FilePath -> Fay ()
showCurrentPath path = do
  getElementById "currentDirectory" >>= setHtml path
  return ()

treeNodeAction :: TreeNode -> Fay ()
treeNodeAction tn = do
  showCurrentPath currentPath
  setCurrentPath currentPath
  setCurrentDirectory currentDirectory
  unless (isDir tn) $
    if isTextFile currentPath then
      readFile currentPath (toAction (readFileAction currentPath))
    else if isImageFile currentPath then do
      getEditor
               >>= removeAllEditorEvent "change"
               >>= setEditorValue (T.concat ["![", text tn, "](", currentPath, ")"])
               >>= setEditorMode "test.md"
               >>= setEditorEvent "change" (const $ updatePreview "test.md")
      setTimeout 100 $ const (updatePreview "test.md")
      return ()
    else do
      getEditor
               >>= removeAllEditorEvent "change"
               >>= setEditorValue (T.concat ["[", text tn, "](", currentPath, ")"])
               >>= setEditorMode "test.md"
               >>= setEditorEvent "change" (const $ updatePreview "test.md")
      setTimeout 100 $ const (updatePreview "test.md")
      return ()

  where currentPath = serverPath tn
        currentDirectory = if isDir tn then currentPath
                           else dropFileName currentPath

selectFile :: (Text -> Text -> Fay ()) -> Fay ()
selectFile = ffi "selectFile(%1)"

uploadFile :: Bool -> Event -> Fay ()
uploadFile isArc _ = selectFile action
  where action :: Text -> Text -> Fay ()
        action name dat = do
          currentDirectory <- getCurrentDirectory
          put (uri </> currentDirectory </> name) dat (const updateTree)
        uri = if isArc then "/api/archive" else "/api/file"

showHowto :: Text -> Fay ()
showHowto = ffi "showHowto(%1)"

loadPublicIdAndShow :: Fay ()
loadPublicIdAndShow = get "/api/publicId" action
  where action :: XMLHttpRequest -> Fay ()
        action xhr = responseText xhr >>= showHowto

setCanPreview :: Bool -> Fay ()
setCanPreview = ffi "(function(can) { window.canPreview = can; })(%1)"

setPreview :: Bool -> Fay ()
setPreview = ffi "(function(can) { window.preview = can; })(%1)"

setDisplay :: Text -> Element -> Fay Element
setDisplay = ffi "(function (val, elem) { elem.style.display = val; return elem })(%1, %2)"

getCanPreview :: Fay Bool
getCanPreview = ffi "window.canPreview"

getPreview :: Fay Bool
getPreview = ffi "window.preview"

showPreview :: Fay ()
showPreview = do
  setPreview True
  getElementById "out" >>= setDisplay "block"
  getElementById "in" >>= flip removeClass "no-preview"
  getElementById "hidePreview" >>= setDisplay "inline-block"
  getElementById "showPreview" >>= setDisplay "none"
  getEditor >>= resize
  return ()

hidePreview :: Fay ()
hidePreview = do
  setPreview False
  getElementById "out" >>= setDisplay "none"
  getElementById "in" >>= flip addClass "no-preview"
  getElementById "hidePreview" >>= setDisplay "none"
  getElementById "showPreview" >>= setDisplay "inline-block"
  getEditor >>= resize
  return ()

disablePreview :: Fay ()
disablePreview = do
  setCanPreview False
  getElementById "hidePreview" >>= setDisplay "none"
  getElementById "showPreview" >>= setDisplay "none"
  getElementById "out" >>= setDisplay "none"
  getElementById "in" >>= flip addClass "no-preview"
  getEditor >>= resize
  return ()

enablePreview :: Fay ()
enablePreview = do
  setCanPreview True
  preview <- getPreview
  if preview then
    showPreview
  else
    hidePreview

canPreviewFile :: FilePath -> Bool
canPreviewFile = ffi "canPreviewFile(%1)"

isMarkdownFile :: FilePath -> Bool
isMarkdownFile = ffi "isMarkdownFile(%1)"

updatePreview :: FilePath -> Fay ()
updatePreview fn = do
  c <- getCanPreview
  if canPreviewFile fn then do
    unless c enablePreview
    body <- getEditor >>= getEditorValue
    let art = parseArticle body
        content = if isMarkdownFile fn then renderMarkdown $ getContent art
                  else getContent art
        title = getTitle art
    if T.null title then
      setOutput content
    else
      setOutput $ T.concat ["<h1>", title, "</h1>", content]
  else
    when c disablePreview

main :: Fay ()
main = do
  getElementById "publish"
      >>= addEventListener "click" publishEvent
  getElementById "new"
      >>= addEventListener "click" newDoc
  getElementById "delete"
      >>= addEventListener "click" deleteDoc
  saveBtn
      >>= addEventListener "click" (const saveCurrent)
  getElementById "upload"
      >>= addEventListener "click" (uploadFile False)
  getElementById "uploadArchive"
      >>= addEventListener "click" (uploadFile True)
  getElementById "showHowto"
      >>= addEventListener "click" (const loadPublicIdAndShow)
  getElementById "showPreview"
      >>= addEventListener "click" (const showPreview)
  getElementById "hidePreview"
      >>= addEventListener "click" (const hidePreview)

  loadTree treeNodeAction
