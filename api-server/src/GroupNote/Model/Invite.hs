{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.Invite where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import GroupNote.DB

$(defineTable
    "invite"
    [''Eq, ''Show])

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Invite)
