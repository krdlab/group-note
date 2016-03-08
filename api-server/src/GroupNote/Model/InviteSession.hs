{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.InviteSession where

import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Database.Relational.Query
import GroupNote.DB
import Data.Int (Int64)

$(defineTable
    "invite_session"
    [''Eq, ''Show])

data InsertInviteSession = InsertInviteSession
    { insSessionId  :: Int64
    , insInviteId   :: Int64
    }
$(makeRecordPersistableDefault ''InsertInviteSession)

piInviteSession :: Pi InviteSession InsertInviteSession
piInviteSession =
    InsertInviteSession |$| sessionId'
                        |*| inviteId'
