module Main where

import Control.Monad.Logger (runStderrLoggingT)

import ChatCore.CoreController

main :: IO ()
main = runStderrLoggingT $ runCoreCtl

