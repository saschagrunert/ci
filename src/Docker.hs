-- | Docker interface
--
-- @since 0.1.0
module Docker
  ( buildIfNeeded
  , removeImage
  , runImage
  ) where

import           Config                    (Config, Docker (Build, Image),
                                            docker)
import           Control.Exception         (SomeException, displayException,
                                            try)
import qualified Control.Foldl             as Fold (head)
import           Control.Lens              ((^.))
import           Data.Text                 (pack, unpack)
import           Filesystem.Path.CurrentOS (fromText)
import           Prelude                   hiding (FilePath)
import           System.Directory          (getTemporaryDirectory)
import           Text.Printf               (printf)
import           Turtle                    (ExitCode (ExitSuccess), Shell, Text,
                                            basename, cd, empty, fold, format,
                                            fp, liftIO, mktempdir, shell,
                                            shellStrictWithErr, touch, using,
                                            writeTextFile)

-- | Build the docker image from the config path if necessary
--
-- @since 0.1.0
buildIfNeeded :: Config -> IO (Either String String)
buildIfNeeded config =
  case config ^. docker of
    Build d -> build d
    Image i -> return $ Right i

-- | Build the provided Dockerfile content
--
-- Returns the build image (Right) on success, otherwise the failure (Left)
--
-- @since 0.1.0
build :: String -> IO (Either String String)
build content = do
  res <-
    try $
    fold
      (do tmp <- liftIO getTemporaryDirectory
          dir <- using $ mktempdir (fromText $ pack tmp) "build"
          cd dir
          let dockerFile = "Dockerfile"
          touch dockerFile
          liftIO . writeTextFile dockerFile $ pack content
          let dirText = format fp $ basename dir
          imageBuilt <- dockerBuild dirText
          cd ".."
          return
            (if imageBuilt == ExitSuccess
               then Just dirText
               else Nothing))
      Fold.head
  return $ deduce res
  where
    deduce :: Either SomeException (Maybe (Maybe Text)) -> Either String String
    deduce (Left e)                = Left $ displayException e
    deduce (Right Nothing)         = err
    deduce (Right (Just Nothing))  = err
    deduce (Right (Just (Just t))) = Right $ unpack t
    err = Left "unable to build docker image"
    dockerBuild :: Text -> Shell ExitCode
    dockerBuild n =
      shell (pack $ printf "docker build -q --pull -t %s ." n) empty

-- | Run a command for the given docker image
--
-- Returns the stdout (Right) on success, otherwise the stderr (Left)
--
-- @since 0.1.0
runImage :: String -> String -> IO ExitCode
runImage cmd image =
  shell
    (pack $
     printf "docker run -v $PWD/work -w /work --rm %s sh -c \"%s\"" image cmd)
    empty

-- | Remove a docker image from the local daemon
--
-- @since 0.1.0
removeImage :: String -> IO (Either String String)
removeImage image = shellStrictEither $ printf "docker rmi %s" image

-- | Convert a strict shell command to Either stderr stdout
--
-- @since 0.1.0
shellStrictEither :: String -> IO (Either String String)
shellStrictEither f =
  shellStrictWithErr (pack f) empty >>=
  (\(code, stdout, stderr) ->
     return $
     if code == ExitSuccess
       then Right $ unpack stdout
       else Left $ unpack stderr)
