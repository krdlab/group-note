{-# LANGUAGE OverloadedStrings #-}

module GroupNote.Types where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Servant

--type CookieHeader = '[Header "Set-Cookie" Text]
--type Authorized   = Header "Authorization" Text

data RegUserParams = RegUserParams
    { regInviteCode :: Text
    }
    deriving (Eq, Show)

instance FromFormUrlEncoded RegUserParams where
    fromFormUrlEncoded ps = RegUserParams <$> lookup' "invite_code"
      where
        lookup' key = case lookup key ps of
            Just v  -> Right v
            Nothing -> Left . unpack $ key <> " not found"

type OidcError = String

instance FromText ByteString where
    fromText = Just . encodeUtf8

instance Exception ServantErr
