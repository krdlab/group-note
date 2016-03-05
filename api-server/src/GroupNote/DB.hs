{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GroupNote.DB where

import Control.Exception (finally, catch, SomeException)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Database.Relational.Query
import Language.Haskell.TH (Q, Dec, TypeQ)
import Language.Haskell.TH.Syntax (Name)
import Database.HDBC (SqlValue, rollback, withTransaction, IConnection)
import Database.HDBC.Record (runQuery')
import Database.HDBC.Session (withConnectionIO)
import Database.Record (FromSql, ToSql)

connect :: IO Connection
connect = connectSqlite3 "groupnote.db"

config :: Config
config =  defaultConfig { normalizedTableName = False }

convTypes :: [(String, TypeQ)]
convTypes =
    [ ("datetime",  [t|LocalTime|])
    , ("varchar",   [t|Text|])
    , ("text",      [t|Text|])
    ]

defineTable :: String -> [Name] -> Q [Dec]
defineTable =
    defineTableFromDB
        connect
        (driverSQLite3 { typeMap = convTypes })
        "main"
--defineTable =
--    defineTableFromDB'
--        connect
--        config
--        (driverSQLite3 { typeMap = convTypes })
--        "main"

select'
    :: (IConnection conn, ToSql SqlValue p, FromSql SqlValue a)
    => conn
    -> Relation p a
    -> p
    -> IO [a]
select' conn r = runQuery' conn (relationalQuery r)

reference :: (Connection -> IO a) -> IO a
reference action = withConnectionIO connect $ \conn -> action conn `finally` doRollback conn
  where
    doRollback conn = rollback conn `catch` (\(_ :: SomeException) -> return ())

transaction :: (Connection -> IO a) -> IO a
transaction action = withConnectionIO connect $ \conn -> withTransaction conn action
