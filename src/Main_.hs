{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Main_
  ( AppInstance
  , _main
  ) where

import Control.Monad
import Data.Vector (Vector, (!?))
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.ByteString.Base64.Lazy

worldBase64 :: String
worldBase64
  =  "V29ybGQge2FwcEluc3RhbmNlcyA9IFtbSU9PcGVyYXRpb24gSU9SZWFkICJub25hbWUiLElPT3Bl"
  ++ "cmF0aW9uIElPV3JpdGUgIkhpLCBub25hbWUhCiJdLFtJT09wZXJhdGlvbiBJT1JlYWQgIlwxMDQy"
  ++ "XDEwNzJcMTA4OVwxMTAzIixJT09wZXJhdGlvbiBJT1dyaXRlICJIaSwgXDEwNDJcMTA3MlwxMDg5"
  ++ "XDExMDMhCiJdLFtJT09wZXJhdGlvbiBJT1JlYWQgIlwxMDU0XDEwODNcMTEwMyIsSU9PcGVyYXRp"
  ++ "b24gSU9Xcml0ZSAiSGksIFwxMDU0XDEwODNcMTEwMyEKIl1dfQo="

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
  name <- getInText app 0
  let greeting = "Hi, " ++ name ++ "!\n"
  let ok = isOutTextEquals greeting app 1
  if ok then return (name ++ "\n" ++ greeting) else Nothing
