{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.Note where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Text
import Data.Time
import Database.Relational.Query
import GroupNote.DB

$(defineTable
    "note"
    [''Eq, ''Show])
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Note)

data NewNoteReq = NewNoteReq
    { reqTitle   :: Text
    , reqContent :: Text
    }
    deriving (Eq, Show)
$(deriveFromJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''NewNoteReq)
