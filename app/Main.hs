module Main where

#include <haskell>
import Lib

main :: IO ()
main = do
  args <- V.fromList <$> getArgs
  case pmain =<< read <$> (args !? 0) of
    Just text -> putStr text
    Nothing -> putStrLn "Error!"
