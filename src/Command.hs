-- | Command handling
--
-- @since 0.1.0
module Command
  ( run
  ) where

import Config (load)
import Docker (buildIfNeeded, removeImage, runImage)
import Turtle (ExitCode (ExitSuccess))
import Utils  ((>>|))

run :: String -> String -> IO (Either String String)
run path cmd =
  load path >>| buildIfNeeded >>|
  (\image -> do
     code <- runImage cmd image
     removeImage image >>|
       return
         (if code == ExitSuccess
            then return . Right $ show code
            else return . Left $ show code))
