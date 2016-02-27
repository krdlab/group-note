{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.Member where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import GroupNote.DB

$(defineTable
    "member"
    [''Eq, ''Show])

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Member)
