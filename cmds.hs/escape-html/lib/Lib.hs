module Lib
(escapeHTML
) where


import qualified Data.Text as T

escapeHTML :: T.Text -> T.Text
escapeHTML = T.concatMap htmlEscape'
  where
    htmlEscape' '<'  = T.pack "&lt;"
    htmlEscape' '>'  = T.pack "&gt;"
    htmlEscape' '&'  = T.pack "&amp;"
    htmlEscape' '"'  = T.pack "&quot;"
    htmlEscape' '\'' = T.pack "&#39;"
    htmlEscape' c    = T.singleton c
