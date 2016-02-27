{-# LANGUAGE OverloadedStrings #-}

module GroupNote (start) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (getEnv)

import GroupNote.Types
import GroupNote.Server

start :: IO ()
start = do
    conf <- AppConf
        <$> (T.pack <$> getEnv "OPENID_ISSUER_LOCATION")
        <*> (B.pack <$> getEnv "OPENID_CLIENT_ID")
        <*> (B.pack <$> getEnv "OPENID_CLIENT_SECRET")
        <*> (B.pack <$> getEnv "OPENID_REDIRECT_URI")
    run 3000 $ app conf

app :: AppConf -> Application
app conf = serve api $ server conf

api :: Proxy API
api = Proxy
