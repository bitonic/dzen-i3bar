{-# LANGUAGE OverloadedStrings #-}
module I3 where

import           Data.Text (Text)

import qualified Data.Aeson as Aeson

data I3 = I3
    { i3FullText  :: Maybe Text
    , i3ShortText :: Maybe Text
    , i3Color     :: Maybe Text
    , i3Name      :: Maybe Text
    , i3Instance  :: Maybe Text
    , i3Urgent    :: Maybe Bool
    }
    deriving (Show, Read, Eq)

toJson :: I3 -> Aeson.Value
toJson I3 { i3FullText  = ft
          , i3ShortText = st
          , i3Color     = c
          , i3Name      = n
          , i3Instance  = i
          , i3Urgent    = u
          } =
    Aeson.object $ concat [ sValue "full_text" ft
                          , sValue "short_text" st
                          , sValue "color" c
                          , sValue "name" n
                          , sValue "instance" i
                          , maybe [] (\t -> [("urgent", Aeson.Bool t)]) u
                          ]
  where
    sValue key = maybe [] (\t -> [(key, Aeson.String t)])