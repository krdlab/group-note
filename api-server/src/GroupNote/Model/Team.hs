{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.Team where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Text (Text)
import GroupNote.DB

$(defineTable
    "team"
    [''Eq, ''Show])
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Team)

data NewTeamReq = NewTeamReq
    { reqIdName :: Text
    , reqName   :: Text
    }
    deriving (Eq, Show)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''NewTeamReq)
