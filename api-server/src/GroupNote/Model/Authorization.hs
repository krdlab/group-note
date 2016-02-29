{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GroupNote.Model.Authorization where

import GroupNote.DB

$(defineTable
    "authorization"
    [''Eq, ''Show])
