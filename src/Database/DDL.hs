{-# LANGUAGE OverloadedStrings #-}

module Database.DDL where

import           Data.Text
import           Database.Connections
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

init :: Text -> IO ()
init name = executeWithConn "tr-en.db" executeDDL
  where
    executeDDL conn = execute_ conn ddl

ddl :: Query
ddl =
  "DROP TABLE IF EXISTS words;\
  \DROP TABLE IF EXISTS translations;\
  \CREATE TABLE words (\
  \  id INTEGER PRIMARY KEY,\
  \  word TEXT NOT NULL UNIQUE,\
  \  annotation TEXT\
  \);\
  \\
  \CREATE TABLE translations (\
  \  id INTEGER PRIMARY KEY,\
  \  word_id INTEGER,\
  \  translation TEXT NOT NULL,\
  \  FOREIGN KEY(word_id) REFERENCES words(id)\
  \);\
  \"
