module Main where

import System.Environment

import ArcGrid

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    fnames -> mapM_ printFile fnames

usage :: IO ()
usage = do
  pname <- getProgName
  putStrLn $ "Usage: " ++ pname ++ " <file ...>"

printFile :: String -> IO ()
printFile fn = (arcGridFromFile fn >>= putStrLn . show)
