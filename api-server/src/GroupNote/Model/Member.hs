{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module GroupNote.Model.Member where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Text (Text)
import GroupNote.DB

$(defineTable
    "member"
    [''Eq, ''Show])
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Member)

data MemberRes = MemberRes
    { resIdName :: Text
    , resName   :: Text
    }
    deriving (Eq, Show)
instance ToJSON MemberRes where
    toJSON m = object [ "id_name" .= resIdName m
                      , "name"    .= resName m
                      ]
