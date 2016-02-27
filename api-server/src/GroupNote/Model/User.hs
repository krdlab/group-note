{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.User where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Text
import Data.Time
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Database.Relational.Query
import GroupNote.DB

$(defineTable
    "user"
    [''Eq, ''Show])

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''User)

data InsertUser = InsertUser
    { insIdName     :: Text
    , insIssuer     :: Text
    , insSub        :: Text
    , insName       :: Text
    , insEmail      :: Text
    , insCreatedAt  :: LocalTime
    }
$(makeRecordPersistableDefault ''InsertUser)

piUser :: Pi User InsertUser
piUser = InsertUser
            |$| idName'
            |*| issuer'
            |*| sub'
            |*| name'
            |*| email'
            |*| createdAt'
