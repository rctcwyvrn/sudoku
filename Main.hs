module Main where

import Sudoku (initialize, solve, printGrid)

main :: IO ()
main = do
  grid <- getContents
  putStrLn $ printGrid $ solve $ initialize grid