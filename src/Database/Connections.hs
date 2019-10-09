{-# LANGUAGE OverloadedStrings #-}

module Database.Connections where

import qualified Data.Text              as T
import           Database.SQLite.Simple

executeWithConn :: T.Text -> (Connection -> IO ()) -> IO ()
executeWithConn dbName action = do
  conn <- open $ T.unpack dbName
  action conn
  close conn

queryWithConn :: T.Text -> (Connection -> IO a) -> IO a
queryWithConn dbName query = do
  conn   <- open $ T.unpack dbName
  result <- query conn
  close conn
  return result

databaseFilename :: T.Text -> T.Text
databaseFilename name = T.concat [name, ".db"]
