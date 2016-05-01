{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Main_ where

import Control.Monad
import Data.Vector (Vector, (!?))
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.ByteString.Base64.Lazy

worldBase64 :: String
worldBase64
  =  "V29ybGQge2FwcEluc3RhbmNlcyA9IFtbSU9PcGVyYXRpb24gSU9Xcml0ZSAiV2hhdCBpcyB5b3Vy"
  ++ "IG5hbWU/CiIsSU9PcGVyYXRpb24gSU9SZWFkICJub25hbWUiLElPT3BlcmF0aW9uIElPV3JpdGUg"
  ++ "IkhpLCBub25hbWUhCiJdLFtJT09wZXJhdGlvbiBJT1dyaXRlICJXaGF0IGlzIHlvdXIgbmFtZT8K"
  ++ "IixJT09wZXJhdGlvbiBJT1JlYWQgIlwxMDQyXDEwNzJcMTA4OVwxMTAzIixJT09wZXJhdGlvbiBJ"
  ++ "T1dyaXRlICJIaSwgXDEwNDJcMTA3MlwxMDg5XDExMDMhCiJdLFtJT09wZXJhdGlvbiBJT1dyaXRl"
  ++ "ICJXaGF0IGlzIHlvdXIgbmFtZT8KIixJT09wZXJhdGlvbiBJT1JlYWQgIlwxMDU0XDEwODNcMTEw"
  ++ "MyIsSU9PcGVyYXRpb24gSU9Xcml0ZSAiSGksIFwxMDU0XDEwODNcMTEwMyEKIl1dfQo="

type AppInstance = Int
type IOIndex = Int
data IOAction = IORead | IOWrite deriving (Eq, Show, Read)
data IOOperation = IOOperation IOAction String deriving (Show, Read)
data World = World { appInstances :: Vector (Vector IOOperation) } deriving (Show, Read)

world :: World
world = read $ U.toString $ decodeLenient $ U.fromString worldBase64

getInOutText :: IOAction -> AppInstance -> IOIndex -> Maybe String
getInOutText action app i = do
  IOOperation actual_action result <- (!? i) <=< (!? app) $ appInstances world
  if actual_action == action then return result else Nothing

getInText :: AppInstance -> IOIndex -> Maybe String
getInText = getInOutText IORead

getOutText :: AppInstance -> IOIndex -> Maybe String
getOutText = getInOutText IOWrite

isOutTextEquals :: String -> AppInstance -> IOIndex -> Bool
isOutTextEquals text inst index = getOutText inst index == Just text

_main :: AppInstance -> Maybe String
_main app = do
  let question = "What is your name?\n"
  _ <- if isOutTextEquals question app 0 then return () else Nothing
  name <- getInText app 1
  let greeting = "Hi, " ++ name ++ "!\n"
  _ <- if isOutTextEquals greeting app 2 then return () else Nothing
  return $ question ++ name ++ "\n" ++ greeting
