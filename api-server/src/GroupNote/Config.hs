{-# LANGUAGE OverloadedStrings #-}

module GroupNote.Config where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml

data AppConf = AppConf
    { issuerLocation :: Text
    , clientId       :: ByteString
    , clientSecret   :: ByteString
    , redirectUri    :: ByteString
    }
    deriving (Show, Eq)

instance FromJSON AppConf where
    parseJSON (Object o) = AppConf
        <$> o .: "issuer_location"
        <*> o `bs` "client_id"
        <*> o `bs` "client_secret"
        <*> o `bs` "redirect_uri"
      where
        bs o' f = encodeUtf8 <$> o' .: f
    parseJSON _ = error "cannot parse AppConf from YAML"

load :: FromJSON a => FilePath -> IO (Either ParseException a)
load = decodeFileEither
