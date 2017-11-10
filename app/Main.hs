module Main where

import ArcGrid

testFile = "app/valley.asc"

main :: IO ()
main = arcGridFromFile testFile >>= putStrLn . show
