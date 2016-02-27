{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GroupNote.Server.Combinators where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (split)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Network.HTTP.Types (status403)
import Network.Wai (requestHeaders)
import Servant (FromText(..), HasServer(..), ServerT, (:>), Proxy(..))
import Servant.Server.Internal (failWith, RouteMismatch(..))
import Web.Cookie (parseCookiesText)

import qualified GroupNote.Model as Model
import GroupNote.Model.User (User)

data Cookie (sym :: Symbol) a

-- https://github.com/haskell-servant/servant/pull/88
instance (KnownSymbol sym, FromText a, HasServer sublayout)
    => HasServer (Cookie sym a :> sublayout)
  where
    type ServerT (Cookie sym a :> sublayout) m = Maybe a -> ServerT sublayout m

    route Proxy subserver request respond = do
        let mheader = lookup "cookie" (requestHeaders request) :: Maybe ByteString
            mvalue  = fromText =<< lookup key =<< fmap parseCookiesText mheader
        route (Proxy :: Proxy sublayout) (subserver mvalue) request respond
      where
        key = fromString $ symbolVal (Proxy :: Proxy sym)

data Authorized a

instance (HasServer sublayout)
    => HasServer (Authorized User :> sublayout)
  where
    type ServerT (Authorized User :> sublayout) m = User -> ServerT sublayout m

    route Proxy subserver request respond = do
        let mheader = lookup "authorization" (requestHeaders request) :: Maybe ByteString
            mvalue  = parseAccessToken mheader
        case mvalue of
            Just value ->  do
                muser <- liftIO $ Model.findUserByAccessToken (decodeUtf8 value)
                case muser of
                    Just user ->
                        route (Proxy :: Proxy sublayout) (subserver user) request respond
                    Nothing   ->
                        respond $ failWith NotFound
            Nothing    ->
                respond $ failWith $ HttpError status403 Nothing
      where
        parseAccessToken m = m >>= second . split ' '
          where
            second ["Bearer", x] = Just x
            second _             = Nothing
