{-# LANGUAGE OverloadedStrings #-}
module I3 where

import Text.JSON

data I3 = I3
    { fullText  :: Maybe String
    , shortText :: Maybe String
    , color     :: Maybe String
    , name      :: Maybe String
    , instance_ :: Maybe String
    , urgent    :: Maybe Bool
    }
    deriving (Show, Read, Eq)

default_ :: I3
default_ = I3 Nothing Nothing Nothing Nothing Nothing Nothing

toJson :: I3 -> JSValue
toJson I3 { fullText  = ft
          , shortText = st
          , color     = c
          , name      = n
          , instance_ = i
          , urgent    = u
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