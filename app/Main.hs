module Main where

import Lib
import qualified PokeTry1
import qualified PokeTry2
import qualified PokeTry3
import qualified MyLogger2

main :: IO ()
main = do
  someFunc
  putStrLn "-------------------------------------------------"
  PokeTry1.main
  putStrLn "-------------------------------------------------"
  PokeTry2.main
  putStrLn "-------------------------------------------------"
  PokeTry3.main
  putStrLn "-------------------------------------------------"
  MyLogger2.main
