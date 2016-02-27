{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.Session where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import GroupNote.DB

$(defineTable
    "session"
    [''Eq, ''Show])

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Session)
