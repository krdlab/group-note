{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.SessionWithInvite where

import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Database.Relational.Query
import GroupNote.DB
import Data.Int (Int64)

$(defineTable
    "session_with_invite"
    [''Eq, ''Show])

data InsertSessionWithInvite = InsertSessionWithInvite
    { insSessionId  :: Int64
    , insInviteId   :: Int64
    }
$(makeRecordPersistableDefault ''InsertSessionWithInvite)

piSessionWithInvite :: Pi SessionWithInvite InsertSessionWithInvite
piSessionWithInvite =
    InsertSessionWithInvite |$| sessionId'
                            |*| inviteId'
