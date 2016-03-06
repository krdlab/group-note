{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GroupNote.Model.Note where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Int (Int64)
import Data.Text
import GroupNote.DB

$(defineTable
    "note"
    [''Eq, ''Show])
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Note)

data NewNoteReq = NewNoteReq
    { newTitle   :: Text
    , newContent :: Text
    }
    deriving (Eq, Show)
instance FromJSON NewNoteReq where
    parseJSON = withObject "NewNoteReq" $ \o ->
        NewNoteReq <$> o .: "title" <*> o .: "content"

data UpdateNoteReq = UpdateNoteReq
    { updateTitle   :: Text
    , updateContent :: Text
    , updateVersion :: Int64
    }
    deriving (Eq, Show)
instance FromJSON UpdateNoteReq where
    parseJSON = withObject "UpdateNoteReq" $ \o ->
        UpdateNoteReq <$> o .: "title" <*> o .: "content" <*> o .: "version"
