-- | The main development
--
-- @since 0.1.0
module Devel
  ( main
  ) where

import Web.Application (develMain)

main :: IO ()
main = develMain
