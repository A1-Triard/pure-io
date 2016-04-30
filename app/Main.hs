module Main where

import Lib

main :: IO ()
main = do
  s <- getLine
  putStrLn $ show $ pmain $ read s
