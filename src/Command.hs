-- | Command handling
--
-- @since 0.1.0
module Command
  ( run
  ) where

import Config       (Step, command, load, pipeline, steps)
import Control.Lens ((^.))
import Docker       (buildIfNeeded, removeImage, runImage)
import Turtle       (ExitCode (ExitFailure, ExitSuccess))
import Utils        ((>>|))

run :: String -> IO (Either String String)
run path =
  load path >>|
  (\config ->
     buildIfNeeded config >>|
     (\image -> do
        code <- runPipeline (config ^. pipeline) image ExitSuccess
        removeImage image >>|
          return
            (if code == ExitSuccess
               then return $ Right "Success"
               else return $ Left "Failure")))

-- | Run the complete pipeline
--
-- @since 0.1.0
runPipeline :: [Step] -> String -> ExitCode -> IO ExitCode
runPipeline (x:xs) image ExitSuccess = runStep x image >>= runPipeline xs image
runPipeline _ _ err@(ExitFailure _) = return err
runPipeline [] _ _ = return ExitSuccess

-- | Run a single step
--
-- @since 0.1.0
runStep :: Step -> String -> IO ExitCode
runStep step image = do
  res <- runImage (step ^. command) image
  case step ^. steps of
    Nothing   -> return res
    Just more -> runPipeline more image ExitSuccess
