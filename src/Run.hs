-- | Run command handling
--
-- @since 0.1.0
module Run
  ( runConfig
  ) where

import Config                   (Step, command, load, pipeline, steps)
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens             ((^.))
import Docker                   (buildIfNeeded, removeImage, runImage)
import Turtle                   (ExitCode (ExitFailure, ExitSuccess))
import Utils                    ((>>|))

-- | Run a configuration
--
-- @since 0.1.0
runConfig :: String -> IO (Either String String)
runConfig path =
  load path >>|
  (\config ->
     buildIfNeeded config >>|
     (\image -> do
        success <- runPipeline (config ^. pipeline) image
        removeImage image >>|
          return
            (if success
               then return $ Right "Success"
               else return $ Left "Failure")))

-- | Run the complete pipeline
--
-- @since 0.1.0
runPipeline :: [Step] -> String -> IO Bool
runPipeline stepList image = do
  exitCodes <- mapConcurrently (runStep image) stepList
  return $ all (== True) exitCodes

-- | Run a single step
--
-- @since 0.1.0
runStep :: String -> Step -> IO Bool
runStep image step = do
  res <- runImage (step ^. command) image
  case (step ^. steps, res) of
    (_, ExitFailure _)       -> return False
    (Nothing, ExitSuccess)   -> return True
    (Just more, ExitSuccess) -> runPipeline more image
