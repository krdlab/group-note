{-# LANGUAGE OverloadedStrings #-}

module GroupNote
    ( start
    , Options(..)
    ) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

import GroupNote.Config (load, AppConf(..))
import GroupNote.Server

data Options = Options
    { optionConfigPath :: FilePath
    , optionDebugMode  :: Bool
    }
    deriving (Eq, Show)

start :: Options -> IO ()
start opts = do
    print opts
    let path  = optionConfigPath opts
        debug = optionDebugMode opts
    res  <- load path
    conf <- case res of
        Right c -> return c
        Left  e -> error . show $ e
    -- TODO: CORS
    run 3000 $ app conf

app :: AppConf -> Application
app conf = serve api $ server conf

api :: Proxy API
api = Proxy
