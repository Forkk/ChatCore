module Main where



import ChatCore.CoreController
import ChatCore.UserController
import ChatCore.NetworkController
import ChatCore.Events
import ChatCore.Protocol
import ChatCore.Protocol.JSON
import ChatCore.Types

main :: IO ()
main = runCoreCtl

