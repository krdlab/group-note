{-# LANGUAGE OverloadedStrings #-}

module GroupNote
    ( start
    , Options(..)
    ) where

import Data.Monoid ((<>))
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..), simpleResponseHeaders)
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
    let path  = optionConfigPath opts
        debug = optionDebugMode opts
    conf <- load' path
    run 3000 $ stack (middles debug) (app conf)
  where
    load' path = either (error . show) id <$> load path

stack :: [Middleware] -> Application -> Application
stack ms a = foldr id a ms

middles :: Bool -> [Middleware]
middles debug
    | debug     = [logStdoutDev, cors']
    | otherwise = [logStdout]

cors' :: Middleware
cors'  = cors $ const (Just policy)
  where
    policy = simpleCorsResourcePolicy {
          corsMethods = ["GET", "POST", "PUT", "DELETE"]
        , corsRequestHeaders = simpleResponseHeaders <> ["Authorization"]
        }

app :: AppConf -> Application
app conf = serve api $ server conf

api :: Proxy API
api = Proxy
