module Main where

import System.Environment (getArgs)

import Parser (parseFile, pprint)

main :: IO ()
main = do
  [file] <- getArgs
  res <- parseFile file
  putStrLn (pprint res)

