-- | The main production executable
--
-- @since 0.1.0
module Main
  ( main
  ) where

import Web.Application (appMain)

main :: IO ()
main = appMain
