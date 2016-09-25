{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module File
  (
    saveFile,
    readFile,
    deleteFile
  ) where

import Prelude hiding (concat)
import Data.Text (fromString, Text, pack, concat)
import DOM (XMLHttpRequest, responseText, status)
import FilePath ((</>), FilePath)
import HTTP (get, put, delete)

fromAction :: (Either Text Text -> Fay ()) -> XMLHttpRequest -> Fay ()
fromAction act = handler
  where handler :: XMLHttpRequest -> Fay ()
        handler xhr = do
          st <- status xhr
          rt <- responseText xhr
          if st >= 400 then
            act (Left (concat ["XHR returned status ", pack (show st), ":\n", rt]))
          else
            act (Right rt)

saveFile :: FilePath -> Text -> (Either Text Text -> Fay ()) -> Fay ()
saveFile fn body act = put url body handler
  where handler = fromAction act
        url = "/api/file" </> fn

readFile :: FilePath -> (Either Text Text -> Fay ()) -> Fay ()
readFile fn act = get url handler
  where handler = fromAction act
        url = "/api/file" </> fn

deleteFile :: FilePath -> (Either Text Text -> Fay ()) -> Fay ()
deleteFile fn act = delete ur handler
  where handler = fromAction act
        ur = "/api/file" </> fn
