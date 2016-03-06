{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GroupNote.Server.Resource where

import Control.Exception (Exception)
import Control.Monad.Catch (catches, Handler(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Text (Text)
import Servant

import GroupNote.Server.Combinators
import qualified GroupNote.Model as Model
import GroupNote.Model (TeamId, NoteId)
import GroupNote.Model.Member (MemberRes)
import GroupNote.Model.Note (Note, NewNoteReq, UpdateNoteReq)
import GroupNote.Model.Team (Team, NewTeamReq, UpdateTeamReq)
import GroupNote.Model.User (User)
import qualified GroupNote.Model.User as User

type API =
         Authorized User :> "teams" :> Get '[JSON] [Team]
    :<|> Authorized User :> "teams" :> ReqBody '[JSON] NewTeamReq :> Post '[JSON] Team
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> ReqBody '[JSON] UpdateTeamReq :> Put '[JSON] Team
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> Delete '[] ()
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "invite-code" :> Post '[JSON] Text
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "members" :> Get '[JSON] [MemberRes]
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "notes" :> Get '[JSON] [Note]
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "notes" :> ReqBody '[JSON] NewNoteReq :> Post '[JSON] Note
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "notes" :> Capture "nid" NoteId :> Get '[JSON] Note
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "notes" :> Capture "nid" NoteId :> ReqBody '[JSON] UpdateNoteReq :> Put '[JSON] Note
    :<|> Authorized User :> "teams" :> Capture "tid" TeamId :> "notes" :> Capture "nid" NoteId :> Delete '[] ()

server :: Server API
server =
         getTeams :<|> postTeams :<|> putTeam :<|> deleteTeam
    :<|> postInvite
    :<|> getMembers
    :<|> getNotes
    :<|> postNote
    :<|> getNote :<|> putNote :<|> deleteNote

getTeams :: User -> EitherT ServantErr IO [Team]
getTeams u = liftIO $ Model.listTeams (User.id u)

postTeams :: User -> NewTeamReq -> EitherT ServantErr IO Team
postTeams u r = liftIO $ Model.createTeam (User.id u) r

putTeam :: User -> TeamId -> UpdateTeamReq -> EitherT ServantErr IO Team
putTeam u tid ut = liftIO (Model.updateTeam (User.id u) tid ut) `catches` handlers

deleteTeam :: User -> TeamId -> EitherT ServantErr IO ()
deleteTeam u tid = liftIO (Model.deleteTeam (User.id u) tid) `catches` handlers

postInvite :: User -> TeamId -> EitherT ServantErr IO Text
postInvite u tid = liftIO (Model.createInvite (User.id u) tid) `catches` handlers

getMembers :: User -> TeamId -> EitherT ServantErr IO [MemberRes]
getMembers u tid = liftIO $ Model.listMembers (User.id u) tid

getNotes :: User -> TeamId -> EitherT ServantErr IO [Note]
getNotes u tid = liftIO $ Model.listNotes (User.id u) tid

postNote :: User -> TeamId -> NewNoteReq -> EitherT ServantErr IO Note
postNote u tid n = liftIO (Model.createNote (User.id u) tid n) `catches` handlers

getNote :: User -> TeamId -> NoteId -> EitherT ServantErr IO Note
getNote u tid nid = liftIO (Model.getNote (User.id u) tid nid) `catches` handlers

putNote :: User -> TeamId -> NoteId -> UpdateNoteReq -> EitherT ServantErr IO Note
putNote u tid nid note = liftIO (Model.updateNote (User.id u) tid nid note) `catches` handlers

deleteNote :: User -> TeamId -> NoteId -> EitherT ServantErr IO ()
deleteNote u tid nid = liftIO (Model.deleteNote (User.id u) tid nid) `catches` handlers

--- helpers

response :: Exception err => ServantErr -> err -> EitherT ServantErr IO a
response se err = response' se $ show err

response' :: ServantErr -> String -> EitherT ServantErr IO a
response' se str = left se {errBody = BLC.pack str}

handlers :: [Handler (EitherT ServantErr IO) a]
handlers = map Handler
    [ \e@(Model.Forbidden _)        -> response err403 e
    , \e@(Model.ResourceNotFound _) -> response err404 e
    ]
