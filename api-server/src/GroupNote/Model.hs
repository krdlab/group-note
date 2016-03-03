{-# LANGUAGE OverloadedStrings #-}

module GroupNote.Model where

import Control.Exception (Exception)
import Control.Monad (when, void)
import Control.Monad.Catch (throwM)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (utcToLocalTime, getCurrentTimeZone, getCurrentTime, LocalTime)
import Database.HDBC (IConnection)
import Database.HDBC.Record (runQuery', runInsert, runUpdate, runDelete)
import Database.Relational.Query
import qualified Web.OIDC.Client as O

import GroupNote.DB
import qualified GroupNote.OpenId as OpenId
import GroupNote.Random

import qualified GroupNote.Model.Invite as Invite
import GroupNote.Model.Invite (Invite, invite)
import qualified GroupNote.Model.Session as Session
import GroupNote.Model.Session (Session, session)
import qualified GroupNote.Model.SessionWithInvite as SessionWithInvite
import GroupNote.Model.SessionWithInvite (sessionWithInvite, piSessionWithInvite, InsertSessionWithInvite(..))
import qualified GroupNote.Model.Team as Team
import GroupNote.Model.Team (Team, team, NewTeamReq(..))
import qualified GroupNote.Model.Member as Member
import GroupNote.Model.Member (member, MemberRes(..))
import qualified GroupNote.Model.User as User
import GroupNote.Model.User (User, user, InsertUser(..), piUser)
import qualified GroupNote.Model.UserSession as UserSession
import GroupNote.Model.UserSession (userSession, InsertUserSession(..), piUserSession)
import qualified GroupNote.Model.SessionState as SessionState
import GroupNote.Model.SessionState (SessionState, sessionState)
import qualified GroupNote.Model.Authorization as Authorization
import GroupNote.Model.Authorization (authorization)

data ModelError =
      Forbidden String
    | ResourceNotFound String
    deriving (Eq, Show)

instance Exception ModelError

type InviteCode = Text
type SessionToken = Text
type AccessToken = Text
type UserId = Int64
type TeamId = Int64

findUserByAccessToken :: AccessToken -> IO (Maybe User)
findUserByAccessToken token = reference $ \conn ->
    headMaybe <$> select' conn queryUserByAccessToken token

findUserBySession :: SessionToken -> IO (Maybe User)
findUserBySession token = reference $ \conn ->
    headMaybe <$> select' conn queryUserBySessionToken token

saveAccessToken :: User -> AccessToken -> IO ()
saveAccessToken u t = do
    time <- getCurrentLocalTime
    transaction $ \conn ->
        void $ runInsert conn (insertAuthorization (User.id u) t time) ()
  where
    insertAuthorization uid token time = derivedInsertValue $ do
        Authorization.userId'       <-# value uid
        Authorization.accessToken'  <-# value token
        Authorization.createdAt'    <-# value time
        return unitPlaceHolder

startAuthSession :: SessionToken -> O.State -> Maybe InviteCode -> IO ()
startAuthSession token state mcode = transaction $ \conn -> do
    s <- saveSession' conn token
    saveState' conn s state
    case mcode of
        Just code -> do
            setRegistrationMode' conn token True
            minv <- findInvite conn code
            case minv of
                Just i  -> associateInvite' conn s i
                Nothing -> return ()
        Nothing -> return ()
  where
    findInvite conn code = headMaybe <$> select' conn queryInviteByCode code

saveSession' :: IConnection conn => conn -> SessionToken -> IO Session
saveSession' conn token = do
    time <- getCurrentLocalTime
    void $ runInsert conn (insertSession token time) ()
    head <$> select' conn querySessionByToken token
  where
    insertSession tkn tim = derivedInsertValue $ do
        Session.token'          <-# value tkn
        Session.registration'   <-# value 0
        Session.createdAt'      <-# value tim
        return unitPlaceHolder

saveState' :: IConnection conn => conn -> Session -> ByteString -> IO ()
saveState' conn s state = do
    let sessionId = Session.id s
        state'    = decodeUtf8 state
    void $ runInsert conn (insertState sessionId state') ()
  where
    insertState sid st = derivedInsertValue $ do
        SessionState.sessionId' <-# value sid
        SessionState.token'     <-# value st
        return unitPlaceHolder

associateInvite' :: IConnection conn => conn -> Session -> Invite -> IO ()
associateInvite' conn s i = do
    let ins = InsertSessionWithInvite (Session.id s) (Invite.id i)
    void $ runInsert conn insertSessionWithInvite ins
  where
    insertSessionWithInvite = derivedInsert piSessionWithInvite

getStateBySessionToken :: SessionToken -> IO ByteString
getStateBySessionToken token = reference $ \conn -> do
    ss <- select' conn queryStateBySessionToken token
    when (null ss) $
        throwM $ ResourceNotFound "state"
    return . encodeUtf8 . SessionState.token . head $ ss

getSessionByToken' :: IConnection conn => conn -> SessionToken -> IO Session
getSessionByToken' conn token = do
    ss <- select' conn querySessionByToken token
    when (null ss) $
        throwM $ ResourceNotFound "session"
    return . head $ ss

setRegistrationMode' :: IConnection conn => conn -> SessionToken -> Bool -> IO ()
setRegistrationMode' conn token mode =
    void $ runUpdate conn updateSession (toInt mode, token)
  where
    updateSession = derivedUpdate $ \proj -> do
        (phMod, ()) <- placeholder (\ph -> Session.registration' <-# ph)
        (phSid, ()) <- placeholder (\ph -> wheres $ proj ! Session.token' .=. ph)
        return (phMod >< phSid)
    toInt True  = 1
    toInt False = 0

isSessionRegistrationMode :: SessionToken -> IO Bool
isSessionRegistrationMode token = reference $ \conn ->
    toBool . Session.registration <$> getSessionByToken' conn token
  where
    toBool 0 = False
    toBool _ = True

registerAndJoin :: SessionToken -> O.IssuerLocation -> OpenId.UserInfo -> IO User
registerAndJoin token issuer ui = transaction $ \conn -> do
    s  <- getSessionByToken' conn token
    u  <- registerUser' conn s issuer ui
    mt <- findInvitedTeam' conn token
    case mt of
        Just t  -> joinTeam' conn u t
        Nothing -> return ()
    setRegistrationMode' conn token False
    return u
  where
    findInvitedTeam' conn t =
        headMaybe <$> select' conn queryInvitedTeamBySessionToken t

registerUser' :: IConnection conn => conn -> Session -> Text -> OpenId.UserInfo -> IO User
registerUser' conn s issuer ui = do
    idname <- decodeUtf8 <$> genToken 64
    time   <- getCurrentLocalTime
    let insUser = InsertUser
                    idname
                    issuer
                    (OpenId.sub ui)
                    (OpenId.name ui)
                    (OpenId.email ui)
                    time
    _ <- runInsert conn (derivedInsert piUser) insUser
    u <- head <$> runQuery' conn (relationalQuery queryUserByUname) idname
    let insUserSession = InsertUserSession (Session.id s) (User.id u)
    _ <- runInsert conn (derivedInsert piUserSession) insUserSession
    return u

joinTeam' :: IConnection conn => conn -> User -> Team -> IO ()
joinTeam' conn u t = do
    let uid = User.id u
        tid = Team.id t
    void $ runInsert conn (insertMember uid tid) ()
  where
    insertMember uid tid = derivedInsertValue $ do
        Member.userId' <-# value uid
        Member.teamId' <-# value tid
        return unitPlaceHolder

login :: Text -> Text -> Text -> IO (Either String User)
login sid iss sub = transaction $ \conn -> do
    us <- select' conn queryUserByOpenId (iss, sub)
    if null us
        then return . Left $ "user not found"
        else do
            ss <- select' conn querySessionByToken sid
            if null ss
                then
                    return . Left $ "session not found"
                else do
                    let u = head us
                        s = head ss
                        insUserSession = InsertUserSession (Session.id s) (User.id u)
                    _ <- runInsert conn (derivedInsert piUserSession) insUserSession
                    return . Right $ u

listTeams :: UserId -> IO [Team]
listTeams uid = reference $ \conn -> select' conn queryTeamByUserId uid

createTeam :: UserId -> NewTeamReq -> IO Team
createTeam uid req = do
    let iname = Team.reqIdName req
        dname = Team.reqName req
    time <- getCurrentLocalTime
    transaction $ \conn -> do
        _ <- runInsert conn (insertTeam iname dname uid time) ()
        t <- head <$> select' conn queryTeamByIdName iname
        let tid = Team.id t
        _ <- runInsert conn (insertMember tid uid time) ()
        return t
  where
    insertTeam iname dname oid time = derivedInsertValue $ do
        Team.idName'    <-# value iname
        Team.name'      <-# value dname
        Team.ownerId'   <-# value oid
        Team.createdAt' <-# value time
        Team.updatedAt' <-# value time
        Team.version'   <-# value 1
        return unitPlaceHolder
    insertMember tid uid' time = derivedInsertValue $ do
        Member.teamId'      <-# value tid
        Member.userId'      <-# value uid'
        Member.createdAt'   <-# value time
        return unitPlaceHolder

deleteTeam :: UserId -> TeamId -> IO ()
deleteTeam uid tid = transaction $ \conn -> do
    ts <- select' conn queryTeamByIdAndOwnerId (tid, uid)
    when (null ts) $
        throwM $ Forbidden "cannot delete"
    let t = head ts
    void $ runDelete conn target (Team.id t)
  where
    target = derivedDelete $ \proj ->
        fst <$> placeholder (\ph -> wheres $ proj ! Team.id' .=. ph)

createInvite :: UserId -> TeamId -> IO InviteCode
createInvite uid tid = do
    code <- decodeUtf8 <$> genToken 32
    transaction $ \conn -> do
        ts <- select' conn queryTeamByIdAndOwnerId (tid, uid)
        when (null ts) $
            throwM $ Forbidden "user is not owner"
        void $ runInsert conn (insertInvite code tid) ()
    return code
  where
    insertInvite code' tid' = derivedInsertValue $ do
        Invite.code'    <-# value code'
        Invite.teamId'  <-# value tid'
        return unitPlaceHolder

listMembers :: UserId -> TeamId -> IO [MemberRes]
listMembers uid tid = reference $ \conn ->
    map toRes <$> select' conn queryUserByTeamIdAndOwnerId (tid, uid)
  where
    toRes u = MemberRes (User.idName u) (User.name u)

-- relations

queryUserByAccessToken :: Relation AccessToken User
queryUserByAccessToken = relation' . placeholder $ \ph -> do
    a <- query authorization
    u <- query user
    on $ a ! Authorization.userId' .=. u ! User.id'
    wheres $ a ! Authorization.accessToken' .=. ph
    return u

queryUserByOpenId :: Relation (Text, Text) User
queryUserByOpenId = relation' . placeholder $ \ph -> do
    u <- query user
    wheres $ u ! User.issuer' .=. ph ! fst' `and'` u ! User.sub' .=. ph ! snd'
    return u

queryUserByUname :: Relation Text User
queryUserByUname = relation' . placeholder $ \ph -> do
    u <- query user
    wheres $ u ! User.idName' .=. ph
    return u

queryInvitedTeamBySessionToken :: Relation Text Team
queryInvitedTeamBySessionToken = relation' $ do
    (phSession, i) <- query' queryInviteBySessionToken
    t <- query team
    on $ i ! Invite.teamId' .=. t ! Team.id'
    return (phSession, t)

queryInviteBySessionToken :: Relation Text Invite
queryInviteBySessionToken = relation' $ do
    (phSession, s) <- query' querySessionByToken
    swi <- query sessionWithInvite
    i <- query invite
    on $ s ! Session.id' .=. swi ! SessionWithInvite.sessionId'
    on $ swi ! SessionWithInvite.inviteId' .=. i ! Invite.id'
    return (phSession, i)

queryUserBySessionToken :: Relation Text User
queryUserBySessionToken = relation' $ do
    (phSession, s) <- query' querySessionByToken
    us <- query userSession
    u  <- query user
    on $ s ! Session.id' .=. us ! UserSession.sessionId'
    on $ us ! UserSession.userId' .=. u ! User.id'
    return (phSession, u)

queryInviteByCode :: Relation Text Invite
queryInviteByCode = relation' . placeholder $ \ph -> do
    i <- query invite
    wheres $ i ! Invite.code' .=. ph
    return i

querySessionByToken :: Relation Text Session
querySessionByToken = relation' . placeholder $ \ph -> do
    s <- query session
    wheres $ s ! Session.token' .=. ph
    return s

queryStateBySessionToken :: Relation SessionToken SessionState
queryStateBySessionToken = relation' . placeholder $ \ph -> do
    s  <- query session
    ss <- query sessionState
    on $ s ! Session.id' .=. ss ! SessionState.sessionId'
    wheres $ s ! Session.token' .=. ph
    return ss

queryTeamByUserId :: Relation UserId Team
queryTeamByUserId = relation' . placeholder $ \ph -> do
    m <- query member
    t <- query team
    on $ m ! Member.teamId' .=. t ! Team.id'
    wheres $ m ! Member.userId' .=. ph
    asc $ t ! Team.name'
    return t

queryTeamByIdName :: Relation Text Team
queryTeamByIdName = relation' . placeholder $ \ph -> do
    t <- query team
    wheres $ t ! Team.idName' .=. ph
    return t

queryTeamByIdAndOwnerId :: Relation (TeamId, UserId) Team
queryTeamByIdAndOwnerId = relation' . placeholder $ \ph -> do
    t <- query team
    wheres $ t ! Team.id' .=. ph ! fst' `and'` t ! Team.ownerId' .=. ph ! snd'
    return t

queryUserByTeamIdAndOwnerId :: Relation (TeamId, UserId) User
queryUserByTeamIdAndOwnerId = relation' $ do
    (phT, t) <- query' queryTeamByIdAndOwnerId
    m <- query member
    u <- query user
    on $ t ! Team.id' .=. m ! Member.teamId'
    on $ m ! Member.userId' .=. u ! User.id'
    return (phT, u)

-- helpers

headMaybe :: [a] -> Maybe a
headMaybe xs = if null xs then Nothing else Just (head xs)

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
