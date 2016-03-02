{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GroupNote.Server.Resource where

import Control.Exception (Exception, SomeException)
import Control.Monad.Catch (catches, Handler(..))
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
import GroupNote.Model (SessionToken, InviteCode, TeamId)
import GroupNote.Model.Member (MemberRes)
import GroupNote.Model.Team (Team, NewTeamReq(..))
import GroupNote.Model.User (User)
import qualified GroupNote.Model.User as User
import GroupNote.Random
import GroupNote.Types

type API =
         Authorized User :> "teams" :> Get '[JSON] [Team]
    :<|> Authorized User :> "teams" :> ReqBody '[JSON] NewTeamReq :> Post '[JSON] Team
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "invite-code" :> Post '[JSON] Text
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "members" :> Get '[JSON] [MemberRes]

server :: Server API
server = getTeams :<|> postTeams :<|> postInvite :<|> getMembers

getTeams :: User -> EitherT ServantErr IO [Team]
getTeams u = liftIO $ Model.listTeams (User.id u)

postTeams :: User -> NewTeamReq -> EitherT ServantErr IO Team
postTeams u r = liftIO $ Model.createTeam (User.id u) r

postInvite :: User -> TeamId -> EitherT ServantErr IO Text
postInvite u tid = liftIO (Model.createInvite (User.id u) tid) `catches` handlers
  where
    handlers = map Handler
        [ \e@(Model.Forbidden _)        -> response err403 e
        , \e@(Model.ResourceNotFound _) -> response err404 e
        ]

getMembers :: User -> TeamId -> EitherT ServantErr IO [MemberRes]
getMembers u tid = liftIO $ Model.listMembers (User.id u) tid

response :: Exception err => ServantErr -> err -> EitherT ServantErr IO a
response s e = left s {errBody = BLC.pack . show $ e}
