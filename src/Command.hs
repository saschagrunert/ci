-- | Command handling
--
-- @since 0.1.0
module Command
  ( run
  ) where

import Config (load)
import Docker (buildIfNeeded, removeImage, runImage)
import Utils  ((>>|))

run :: String -> String -> IO (Either String String)
run path cmd =
  load path >>| buildIfNeeded >>|
  (\image -> runImage cmd image >> removeImage image)
