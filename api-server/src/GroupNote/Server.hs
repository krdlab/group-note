{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GroupNote.Server where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Maybe (fromMaybe)
import Lucid
import Servant
import Servant.HTML.Lucid

import GroupNote.Server.Combinators
import qualified GroupNote.Model as Model
import GroupNote.Model (SessionToken)
import qualified GroupNote.Server.Auth as Auth
import qualified GroupNote.Server.Resource as Resource
import GroupNote.Types

type API =
         Auth.API
    :<|> "api" :> Resource.API
    :<|> "login" :> Get '[HTML] (Html ())                       -- for debugging
    :<|> Cookie "token" SessionToken :> Get '[HTML] (Html ())   -- for debugging

server :: AppConf -> Server API
server conf = Auth.server conf :<|> Resource.server :<|> testLogin :<|> testHome

testLogin :: EitherT ServantErr IO (Html ())
testLogin = return . doctypehtml_ $ do
    head_ $
        title_ "Group Note Login"
    body_ $ do
        h2_ "Group Note"
        form_ [method_ "post", action_ "/login"] $
            button_ [type_ "submit"] "Sign in with direct"
        form_ [method_ "post", action_ "/register"] $ do
            button_ [type_ "submit"] "Sign up with direct"
            input_ [type_ "text", name_ "invite_code", placeholder_ "invite code"]

testHome :: Maybe SessionToken -> EitherT ServantErr IO (Html ())
testHome token = do
    let token' = fromMaybe "" token
    muser <- liftIO $ Model.findUserBySession token'
    case muser of
        Just user -> return . doctypehtml_ $ do
                head_ $
                    title_ "Login Success"
                body_ $ do
                    h2_ "Success"
                    p_ $ toHtml . show $ user
                    form_ [method_ "post", action_ "/token"] $
                        button_ [type_ "submit"] "New a token"
        Nothing   -> left err404
