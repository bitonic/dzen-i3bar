{-# LANGUAGE OverloadedStrings #-}
module I3 where

import Text.JSON

data I3 = I3
    { i3FullText  :: Maybe String
    , i3ShortText :: Maybe String
    , i3Color     :: Maybe String
    , i3Name      :: Maybe String
    , i3Instance  :: Maybe String
    , i3Urgent    :: Maybe Bool
    }
    deriving (Show, Read, Eq)

toJson :: I3 -> JSValue
toJson I3 { i3FullText  = ft
          , i3ShortText = st
          , i3Color     = c
          , i3Name      = n
          , i3Instance  = i
          , i3Urgent    = u
          } =
    JSObject $ toJSObject $ concat [ sValue "full_text" ft
                                   , sValue "short_text" st
                                   , sValue "color" c
                                   , sValue "name" n
                                   , sValue "instance" i
                                   , maybe [] (\t -> [("urgent", JSBool t)]) u
                                   ]
  where
    sValue :: String -> Maybe String -> [(String, JSValue)]
    sValue key = maybe [] (\t -> [(key, JSString (toJSString t))])