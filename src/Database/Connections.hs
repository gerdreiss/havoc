module Database.Connections where

import           Data.Text
import           Database.SQLite.Simple

executeWithConn :: Text -> (Connection -> IO ()) -> IO ()
executeWithConn dbName action = do
  conn <- open $ unpack dbName
  action conn
  close conn

queryWithConn :: Text -> (Connection -> IO a) -> IO a
queryWithConn dbName query = do
  conn   <- open $ unpack dbName
  result <- query conn
  close conn
  return result
