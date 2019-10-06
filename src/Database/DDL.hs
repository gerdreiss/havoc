{-# LANGUAGE OverloadedStrings #-}

module Database.DDL where

import qualified Data.Text                     as T
import           Database.Connections
import           Database.SQLite.Simple

init :: T.Text -> IO ()
init name = executeWithConn filename executeDDL
 where
  filename = T.unpack $ T.concat [name, ".db"]
  executeDDL conn = do
    execute_ conn ddlWords
    execute_ conn ddlTranslations

ddlWords :: Query
ddlWords =
  "CREATE TABLE IF NOT EXISTS words (id INTEGER PRIMARY KEY, word TEXT NOT NULL UNIQUE, annotation TEXT)"

ddlTranslations :: Query 
ddlTranslations =
  "CREATE TABLE IF NOT EXISTS translations (id INTEGER PRIMARY KEY, word_id INTEGER, translation TEXT NOT NULL, FOREIGN KEY(word_id) REFERENCES words(id))"
