{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module GroupNote.OpenId where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import qualified Web.OIDC.Client as O
import qualified Web.OIDC.Client.Settings as O

data UserInfo = UserInfo
    { sub           :: Text
    , name          :: Text
    , updatedAt     :: Maybe Int32
    , email         :: Text
    , emailVerified :: Maybe Bool
    }
    deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''UserInfo)

getUserInfo :: O.OIDC -> Text -> Manager -> IO UserInfo
getUserInfo oidc token mgr = do
    let ep = unpack $ fromJust $ O.userinfoEndpoint (O.configuration $ O.oidcProvider oidc)
    req <- parseUrl ep
    let req' = req { method = "GET"
                   , requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 token)]
                   }
    res <- httpLbs req' mgr
    let body = responseBody res
    return . fromJust . decode $ body
