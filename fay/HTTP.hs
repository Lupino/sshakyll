{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module HTTP
  (
    FetchOptions(..),
    fetchOptions,
    RequestMethod(..),
    fetch_,
    fetch,
    get,
    post,
    put,
    delete
  ) where

import Prelude
import DOM (XMLHttpRequest, send, xmlHttpRequest)
import FFI (ffi, Defined(Defined, Undefined))
import Data.Text (fromString, Text)

data FetchOptions = FetchOptions { getUrl     :: Text,
                                   getMethod  :: RequestMethod,
                                   getData    :: Defined Text,
                                   getResolve :: XMLHttpRequest -> Fay (),
                                   getReject  :: Text -> Fay () }


fetchOptions :: FetchOptions
fetchOptions = FetchOptions { getUrl     = "",
                              getMethod  = GET,
                              getData    = Undefined,
                              getResolve = const $ return (),
                              getReject  = const $ return () }

data RequestMethod = GET | POST | PUT | DELETE

open :: RequestMethod -> Text -> XMLHttpRequest -> Fay XMLHttpRequest
open = ffi "(function(method, url, xhr) { xhr['open'](method['instance'], url, true); return xhr; })(%1, %2, %3)"

send_ :: Text -> XMLHttpRequest -> Fay ()
send_ = ffi "%2['send'](%1)"

setOnLoadHandler :: (XMLHttpRequest -> Fay ()) -> XMLHttpRequest -> Fay XMLHttpRequest
setOnLoadHandler = ffi "(function(handler, xhr) { xhr['onload'] = function() { handler(xhr); }; return xhr; })(%1, %2)"

setOnErrorHandler :: (Text -> Fay ()) -> XMLHttpRequest -> Fay XMLHttpRequest
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
        sendData :: Defined Text -> XMLHttpRequest -> Fay ()
        sendData (Defined dt) = send_ dt
        sendData Undefined = send


fetch :: Text -> FetchOptions -> (XMLHttpRequest -> Fay ()) -> Fay ()
fetch url opts cb = fetch_ $ opts { getUrl = url, getResolve = cb }

get :: Text -> (XMLHttpRequest -> Fay ()) -> Fay ()
get ur = fetch ur fetchOptions

post :: Text -> Text -> (XMLHttpRequest -> Fay ()) -> Fay ()
post ur dat = fetch ur opts
  where opts = fetchOptions { getData = Defined dat, getMethod = POST }

put :: Text -> Text -> (XMLHttpRequest -> Fay ()) -> Fay ()
put ur dat = fetch ur opts
  where opts = fetchOptions { getData = Defined dat, getMethod = PUT }

delete :: Text -> (XMLHttpRequest -> Fay ()) -> Fay ()
delete ur = fetch ur fetchOptions { getMethod = DELETE }
