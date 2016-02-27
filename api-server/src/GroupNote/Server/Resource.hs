{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GroupNote.Server.Resource where

import Control.Exception (SomeException)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.HTTP.Types as HTTP
import Servant

import GroupNote.Server.Combinators
import qualified GroupNote.OpenId as GO
import qualified GroupNote.Model as Model
import GroupNote.Model (SessionToken, InviteCode)
import GroupNote.Model.Team (Team)
import GroupNote.Model.User (User)
import GroupNote.Random
import GroupNote.Types

type API =
         Authorized User :> "teams" :> Get '[JSON] [Team]
    :<|> Authorized User :> "teams" :> ReqBody '[JSON] ReqTeam :> Post '[JSON] Team

server :: Server API
server = getTeamsH :<|> postTeamsH
  where
    getTeamsH     = getTeams
    postTeamsH    = undefined -- TODO

getTeams :: User -> EitherT ServantErr IO [Team]
getTeams _ = liftIO Model.dummyTeams
