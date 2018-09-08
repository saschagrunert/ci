-- | Docker interface
--
-- @since 0.1.0
module Docker
  ( MonadIO
  , MonadShell
  , build
  , buildFromConfigPath
  , buildsh
  ) where

import Config       (docker, load)
import Control.Lens ((^.))
import Data.Text    as T (intercalate, pack)
import Prelude      hiding (FilePath)
import Turtle       (ExitCode (ExitSuccess), FilePath, Line, Managed, Shell,
                     Text, basename, cd, empty, format, fp, liftIO, mktempdir,
                     rmtree, sh, shell, touch, using, writeTextFile)

-- | Monadic abstraction for IO actions
--
-- @since 0.1.0
class Monad m =>
      MonadIO m
  where
  sh' :: Shell a -> m ()

-- | Monadic abstraction for IO
--
-- @since 0.1.0
instance MonadIO IO where
  sh' = sh

-- | Monadic abstraction for Shell actions
--
-- @since 0.1.0
class Monad m =>
      MonadShell m
  where
  cd' :: FilePath -> m ()
  liftIO' :: IO a -> m a
  rmtree' :: FilePath -> m ()
  shell' :: Text -> Shell Line -> m ExitCode
  touch' :: FilePath -> m ()
  using' :: Managed a -> m a

-- | Monadic abstraction for Shell
--
-- @since 0.1.0
instance MonadShell Shell where
  cd' = cd
  liftIO' = liftIO
  rmtree' = rmtree
  shell' = shell
  touch' = touch
  using' = using

-- | Monadic abstraction for Shell
--
-- @since 0.1.0
buildFromConfigPath path = do
  p <- load path
  case p of
    Right c ->
      case c ^. docker of
        Just d -> build d
        _      -> undefined
    _ -> undefined

-- | Build the provided Dockerfile content within a new shell
--
-- @since 0.1.0
build :: MonadIO m => String -> m ()
build content = sh' $ buildsh content

-- | Build the provided Dockerfile content
--
-- Returns the build image on success
--
-- @since 0.1.0
buildsh :: MonadShell m => String -> m (Maybe Text)
buildsh content = do
  dir <- using' (mktempdir "." "build")
  cd' dir
  let dockerFile = "Dockerfile"
  touch' dockerFile
  liftIO' . writeTextFile dockerFile $ T.pack content
  let dirText = format fp $ basename dir
  res <- dockerBuild dirText
  cd' ".."
  rmtree' dir
  return $
    if res == ExitSuccess
      then Just dirText
      else Nothing

-- | Build the docker image from the current directory
--
-- @since 0.1.0
dockerBuild ::
     MonadShell m
  => Text -- ^ The name:tag of the image
  -> m ExitCode
dockerBuild n =
  shell' (T.intercalate " " ["docker build --pull -t", n, "."]) empty
