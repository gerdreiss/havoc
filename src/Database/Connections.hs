module Database.Connections where

import           Database.SQLite.Simple

executeWithConn :: String -> (Connection -> IO ()) -> IO ()
executeWithConn dbName action = do
  conn <- open dbName
  action conn
  close conn

queryWithConn :: String -> (Connection -> IO a) -> IO a
queryWithConn dbName query = do
  conn <- open dbName
  result <- query conn
  close conn
  return result
