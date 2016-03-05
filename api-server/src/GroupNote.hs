{-# LANGUAGE OverloadedStrings #-}

module GroupNote (start) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (getArgs)

import GroupNote.Config (load, AppConf(..))
import GroupNote.Server

start :: IO ()
start = do
    args <- getArgs
    conf <- case args of
        [path] -> do
            res <- load path
            case res of
                Right c -> return c
                Left  e -> error . show $ e
        _      -> error "usage: api-server <configuration file path>"
    run 3000 $ app conf

app :: AppConf -> Application
app conf = serve api $ server conf

api :: Proxy API
api = Proxy
