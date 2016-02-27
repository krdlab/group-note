{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.UserSession where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Database.Relational.Query
import GroupNote.DB
import Data.Int (Int64)

$(defineTable
    "user_session"
    [''Eq, ''Show])

data InsertUserSession = InsertUserSession
    { insSessionId  :: Int64
    , insUserId     :: Int64
    }
$(makeRecordPersistableDefault ''InsertUserSession)

piUserSession :: Pi UserSession InsertUserSession
piUserSession = InsertUserSession
                    |$| sessionId'
                    |*| userId'
