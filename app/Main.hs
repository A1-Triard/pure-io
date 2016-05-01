{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Main where

import System.Environment
import Data.Vector ((!?))
import qualified Data.Vector as V hiding ((!?))
import Main_

main :: IO ()
main = do
  args <- V.fromList <$> getArgs
  case _main =<< read <$> (args !? 0) of
    Just text -> putStr text
    Nothing -> putStrLn "Error!"
