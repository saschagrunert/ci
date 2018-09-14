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
            (if success == ExitSuccess
               then return $ Right "Success"
               else return $ Left "Failure")))

-- | Run the complete pipeline
--
-- @since 0.1.0
runPipeline :: [Step] -> String -> IO ExitCode
runPipeline stepList image = do
  results <- mapConcurrently (runStep image) stepList
  return $
    if all (== ExitSuccess) results
      then ExitSuccess
      else ExitFailure 1

-- | Run a single step
--
-- @since 0.1.0
runStep :: String -> Step -> IO ExitCode
runStep image step = do
  res <- runImage (step ^. command) image
  case (step ^. steps, res) of
    (_, e@(ExitFailure _))   -> return e
    (Nothing, ExitSuccess)   -> return ExitSuccess
    (Just more, ExitSuccess) -> runPipeline more image
