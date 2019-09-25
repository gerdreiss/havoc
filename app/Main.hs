{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Vocabulary
import qualified Data.List          as L
import qualified Data.Text          as T
import           System.Environment

data Command = Command
  { name :: T.Text
  , args :: [T.Text]
  }

execute :: Command -> IO ()
execute command
  | name command == "add" = putStrLn "Yeni kelime ekleme..."
  | name command == "update" = putStrLn "Bir kelimeyi güncelleme..."
  | name command == "get" = putStrLn "Bir kelime için çevirileri alma..."
  | name command == "list" = putStrLn "Tüm kelimeleri listelemek..."
  | otherwise = putStrLn "Tam olarak ne istiyorsun?..."

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x:xs) -> execute Command {name = T.pack x, args = map T.pack xs}
    _      -> execute Command {name = "que", args = mempty}
