-- | Docker interface
--
-- @since 0.1.0
module Docker
  ( buildFromConfigPath
  ) where

import           Config            (Docker (Build, Image), docker, load)
import           Control.Exception (SomeException, displayException, try)
import qualified Control.Foldl     as Fold (head)
import           Control.Lens      ((^.))
import           Data.Text         as T (intercalate, pack, unpack)
import           Prelude           hiding (FilePath)
import           Turtle            (ExitCode (ExitSuccess), Shell, Text,
                                    basename, cd, empty, fold, format, fp,
                                    liftIO, mktempdir, rmtree, shell, touch,
                                    using, writeTextFile)

-- | Build the docker image from the config path if necessary
--
-- @since 0.1.0
buildFromConfigPath :: String -> IO (Either String String)
buildFromConfigPath path = do
  p <- load path
  case p of
    Right c ->
      case c ^. docker of
        Build d -> build d
        Image i -> return $ Right i
    Left e -> return . Left $ displayException e

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
      (do dir <- using $ mktempdir "/tmp" "build"
          cd dir
          let dockerFile = "Dockerfile"
          touch dockerFile
          liftIO . writeTextFile dockerFile $ T.pack content
          let dirText = format fp $ basename dir
          imageBuilt <- dockerBuild dirText
          cd ".."
          rmtree dir
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
    deduce (Right (Just (Just t))) = Right $ T.unpack t
    err = Left "unable to build docker image"
    dockerBuild :: Text -> Shell ExitCode
    dockerBuild n =
      shell (T.intercalate " " ["docker build --pull -t", n, "."]) empty
