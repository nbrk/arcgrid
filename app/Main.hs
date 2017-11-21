module Main where

import System.Environment
import System.Clock
import Text.Printf

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
printFile fn = do
  profstart <- getTime Monotonic
  ag <- arcGridFromFile fn
  profend <- getTime Monotonic
  printf "Parsed %dx%d VAT (took %d sec)\n" (ncols ag) (nrows ag) (sec profend - sec profstart)
  
