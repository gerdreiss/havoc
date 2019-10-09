{-# LANGUAGE OverloadedStrings #-}

module Database.Connections where

import qualified Data.Text              as T
import           Database.SQLite.Simple
import           System.Environment

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

-- TODO use getEnv to get user's home to store the database file to