{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Main (main) where

import Prelude hiding (null, concat, putStrLn, lines, unlines)
import FFI (ffi)
import DOM (getElementById, addClass, removeClass, Event, Element, Timer,
            XMLHttpRequest, setTimeout, clearTimeout, responseText)
import Data.Text (fromString, Text, null, concat, putStrLn, unpack,
                  splitOn, lines, unlines, isPrefixOf, (<>))
import FilePath ((</>), dropFileName, FilePath)
import HTTP (get, put, post)
import File (readFile, saveFile, deleteFile)


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

  when (Prelude.not (null currentPath) && isUnsave saveState && isTextFile currentPath) $ do
    saving
    editor <- getEditor
    dat <- getEditorValue editor
    saveFile currentPath dat act

  where act :: Either Text Text -> Fay ()
        act (Left _) = unsaved
        act (Right _) = saved

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
        fixed fn False = fn <> ".md"

deleteDoc :: Event -> Fay ()
deleteDoc _ = do
  currentPath <- getCurrentPath
  unless (null currentPath) $ do
    c <- confirm ("Delete " <> currentPath <> " ?")
    when c $ deleteFile currentPath act
  where act :: Either Text Text -> Fay ()
        act (Left err) = putStrLn err
        act (Right _) = updateTree

callback :: XMLHttpRequest -> Fay ()
callback xhr = responseText xhr >>= putStrLn

data Article = Article { getTitle :: Text, getContent :: Text }

trim :: Text -> Text
trim = ffi "%1.trim()"

parseArticle :: Text -> Article
parseArticle txt = parse $ lines txt
  where parse :: [Text] -> Article
        parse xs | hasTagLine xs = parse_ xs
                 | otherwise     = Article { getTitle = "", getContent = concat xs }

        hasTagLine :: [Text] -> Bool
        hasTagLine = foldr ((||) . isTagLine) False

        isTagLine :: Text -> Bool
        isTagLine x = trim x == "---"

        isTitleLine :: Text -> Bool
        isTitleLine x = "title" `isPrefixOf` x'
          where x' = trim x

        parse_ :: [Text] -> Article
        parse_ (x:xs) = if isTitleLine x then
                          Article { getTitle = trim $ getT x, getContent = getC xs }
                        else parse_ xs
        parse_ []     = Article { getTitle = "", getContent = "" }

        getT :: Text -> Text
        getT = last . splitOn ':'
        getC :: [Text] -> Text
        getC (x:xs) = if isTagLine x then unlines xs
                      else getC xs

renderMarkdown :: Text -> Text
renderMarkdown = ffi "window.md.render(%1)"

confirm :: Text -> Fay Bool
confirm = ffi "window.confirm(%1)"

addEventListener :: Text -> (Event -> Fay a) -> Element ->  Fay ()
addEventListener = ffi "(function(evt, func, elem) {elem.addEventListener(evt, func)})(%1, %2, %3)"

publishEvent :: Event -> Fay ()
publishEvent _ = do
  c <- confirm "Publish Now"
  when c $ post "/api/publish" "" callback

data TreeNode = TreeNode { isDir :: Bool, serverPath :: Text, text :: Text }

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

setOutput :: Text -> Fay ()
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

readFileAction :: FilePath -> Either Text Text -> Fay ()
readFileAction _ (Left err) = error $ unpack err
readFileAction fn (Right body) = do
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
      readFile currentPath (readFileAction currentPath)
    else if isImageFile currentPath then do
      getEditor
               >>= removeAllEditorEvent "change"
               >>= setEditorValue (concat ["![", text tn, "](", currentPath, ")"])
               >>= setEditorMode "test.md"
               >>= setEditorEvent "change" (const $ updatePreview "test.md")
      setTimeout 100 $ const (updatePreview "test.md")
      return ()
    else do
      getEditor
               >>= removeAllEditorEvent "change"
               >>= setEditorValue (concat ["[", text tn, "](", currentPath, ")"])
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
    if null title then
      setOutput content
    else
      setOutput $ concat ["<h1>", title, "</h1>", content]
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
