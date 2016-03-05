{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GroupNote.Server.Auth where

import Control.Exception (SomeException)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as HTTP
import Servant
import qualified Web.OIDC.Client as O

import qualified GroupNote.Config as Conf
import GroupNote.Config (AppConf)
import qualified GroupNote.OpenId as OpenId
import qualified GroupNote.Model as Model
import GroupNote.Model (SessionToken, InviteCode)
import GroupNote.Random
import GroupNote.Server.Combinators
import GroupNote.Types

type API =
         "register" :> ReqBody '[FormUrlEncoded] RegUserParams :> Post '[] ()
    :<|> "login" :> Post '[] ()
    :<|> "callback"
            :> Cookie "token" SessionToken
            :> QueryParam "error" OidcError
            :> QueryParam "code" O.Code
            :> QueryParam "state" O.State
            :> Get '[] ()
    :<|> "token" :> Cookie "token" SessionToken :> Post '[JSON] Text

server :: AppConf -> Server API
server conf =  postRegisterH :<|> postLoginH :<|> getCallbackH :<|> postTokenH
  where
    postRegisterH = postRegister conf
    postLoginH    = postLogin conf
    getCallbackH  = getCallback conf
    postTokenH    = postToken

postRegister :: AppConf -> RegUserParams -> EitherT ServantErr IO a
postRegister conf ps = oidcAuth conf (Just . regInviteCode $ ps)

postLogin :: AppConf -> EitherT ServantErr IO a
postLogin conf = oidcAuth conf Nothing

oidcAuth :: AppConf -> Maybe InviteCode -> EitherT ServantErr IO a
oidcAuth conf mcode = do
    (url, token) <- liftIO startAuth
            `catch` (\(e :: SomeException) -> left err400 {errBody = BLC.pack . show $ e})
    responseSeeOther
        (pack . show $ url)
        [setCookieHeader "token" (encodeUtf8 token)]
  where
    startAuth = do
        token <- decodeUtf8 <$> genToken 64
        state <- genToken 64
        Model.startAuthSession token state mcode
        url <- generateUrl state
        return (url, token)
    generateUrl state = do
        mgr  <- newTlsManager
        oidc <- newOIDC conf mgr
        O.getAuthenticationRequestUrl oidc [O.email, O.profile] (Just state) []

getCallback
    :: AppConf
    -> Maybe SessionToken
    -> Maybe OidcError
    -> Maybe O.Code
    -> Maybe O.State
    -> EitherT ServantErr IO ()
getCallback conf sid err code state = do
    liftIO $ printForDebug err code state -- XXX
    sst <- liftIO $ Model.getStateBySessionToken sid' -- TODO
    when (state' /= sst) $
        left err400
    res <- liftIO registerOrLogin
            `catch` (\(e :: SomeException) -> return . Left . show $ e) -- TODO
    case res of
        Left err' -> left err404 { errBody = BLC.pack err' }
        Right _   -> responseFound "/"
  where
    sid'   = fromMaybe "" sid
    code'  = fromMaybe "" code
    state' = fromMaybe "" state
    registerOrLogin = do
        mgr  <- newTlsManager
        oidc <- newOIDC conf mgr
        tokens <- O.requestTokens oidc code' mgr
        regMode <- Model.isSessionRegistrationMode sid'
        if regMode
            then do
                let acc = O.accessToken tokens
                    iss = O.iss . O.claims . O.idToken $ tokens
                ui <- OpenId.getUserInfo oidc acc mgr
                Right <$> Model.registerAndJoin sid' iss ui
            else do
                let c   = O.claims . O.idToken $ tokens
                    iss = O.iss c
                    sub = O.sub c
                Model.login sid' iss sub

postToken :: Maybe SessionToken -> EitherT ServantErr IO Text
postToken token = do
    muser <- liftIO $ Model.findUserBySession token'
    case muser of
        Just user -> liftIO $ do
            access <- decodeUtf8 <$> genToken 64
            Model.saveAccessToken user access
            return access
        Nothing   -> left err401
  where
    token' = fromMaybe "" token

-- helpers

printForDebug :: Maybe OidcError -> Maybe O.Code -> Maybe O.State -> IO ()
printForDebug e c s = printMaybe e >> printMaybe c >> printMaybe s
  where
    printMaybe m = print $ fromMaybe "nothing" m

newTlsManager :: IO HC.Manager
newTlsManager = newManager tlsManagerSettings

getProvider :: AppConf -> HC.Manager -> IO O.Provider
getProvider conf = O.discover (Conf.issuerLocation conf)

newOIDC :: AppConf -> HC.Manager -> IO O.OIDC
newOIDC conf mgr = do
    prov <- getProvider conf mgr
    return $ setCredentials conf $ O.newOIDC prov

setCredentials :: AppConf -> O.OIDC -> O.OIDC
setCredentials conf = O.setCredentials clientId clientSecret redirectUri
  where
    clientId     = Conf.clientId conf
    clientSecret = Conf.clientSecret conf
    redirectUri  = Conf.redirectUri conf

responseFound :: ByteString -> EitherT ServantErr IO a
responseFound loc = left err302 { errHeaders = [("Location", loc)] }

responseSeeOther :: ByteString -> [HTTP.Header] -> EitherT ServantErr IO a
responseSeeOther loc hs = left err303 { errHeaders = ("Location", loc) : hs }
-- https://github.com/haskell-servant/servant/issues/135

setCookieHeader :: ByteString -> ByteString -> HTTP.Header
setCookieHeader key val = ("Set-Cookie", key <> "=" <> val <> "; HttpOnly;")
