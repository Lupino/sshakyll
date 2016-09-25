{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module FilePath
  (
    FilePath,
    append,
    (</>),
    dropFileName
  ) where

import Data.Text (fromString, Text)
import FFI (ffi)

type FilePath = Text

append :: FilePath -> FilePath -> FilePath
append = ffi "(function(f, g) { p = f + '/' + g; return p.replace(/\\/+/g, '/') })(%1, %2)"

(</>) :: FilePath -> FilePath -> FilePath
f </> g = append f g
infixr 5 </>

dropFileName :: FilePath -> FilePath
dropFileName = ffi "(function(fn) { return fn.substr(0, fn.lastIndexOf('/')) })(%1)"
