{-# LANGUAGE OverloadedStrings #-}

module GroupNote
    ( start
    , Options(..)
    ) where

import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
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
    run 3000 $ stack [logger debug] $ app conf

stack :: [Middleware] -> Application -> Application
stack ms a = foldr id a ms

logger :: Bool -> Middleware
logger debug
    | debug     = logStdoutDev
    | otherwise = logStdout

app :: AppConf -> Application
app conf = serve api $ server conf

api :: Proxy API
api = Proxy
