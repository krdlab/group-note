{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.AccessToken where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Database.HDBC.Query.TH (makeRecordPersistableDefault)
import Database.Relational.Query
import GroupNote.DB
import Data.Int (Int64)

$(defineTable
    "access_token"
    [''Eq, ''Show])
