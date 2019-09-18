{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Data.Text
import           Control.Vocabulary

data Command =
  Command
    { name :: Text
    , args :: [Text]
    }

execute :: Command -> IO ()
execute command
  | name command == "add" = putStrLn "Adding new word..."
  | name command == "update" = putStrLn "Updating a word..."
  | name command == "get" = putStrLn "Retrieving translations for a word..."
  | name command == "list" = putStrLn "Listing all words..."

main :: IO ()
main = putStrLn "FPFTW!"
