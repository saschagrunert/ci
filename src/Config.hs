{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Config de/serialization interface
--
-- @since 0.1.0
module Config
  ( Config
  , Docker(Build, Image)
  , MonadIO
  , docker
  , load
  , run
  ) where

import Control.Arrow     (left)
import Control.Exception (displayException)
import Control.Lens      (makeLenses)
import Data.Yaml         (FromJSON (parseJSON), ParseException,
                          decodeFileEither, withObject, (.:), (.:?))
import GHC.Generics      (Generic)

-- | Monadic abstraction
--
-- @since 0.1.0
class Monad m =>
      MonadIO m
  where
  decodeFileEither' :: String -> m (Either ParseException Config)

-- | Monadic abstraction for IO
--
-- @since 0.1.0
instance MonadIO IO where
  decodeFileEither' = decodeFileEither

-- | The main configuration data
--
-- @since 0.1.0
data Config = Config
  { _docker :: Docker -- ^ The docker confiuration
  , _run    :: String -- ^ The run command
  } deriving (Show, Generic)

-- | The docker config data
--
-- @since 0.1.0
data Docker
  = Build String -- ^ Dockerfile content
  | Image String -- ^ A given image
  deriving (Show, Generic)

makeLenses ''Config

-- | Configuration parsing
--
-- @since 0.1.0
instance FromJSON Config where
  parseJSON =
    withObject "Config" $ \o -> do
      dockerField <- o .:? "docker"
      imageField <- o .:? "image"
      _docker <-
        case (dockerField, imageField) of
          (Nothing, Nothing) -> fail "Neither 'docker' nor 'image' found"
          (Just _, Just _)   -> fail "Both 'docker' and 'image' found"
          (Nothing, Just r)  -> return $ Image r
          (Just l, Nothing)  -> return $ Build l
      _run <- o .: "run"
      return Config {_docker = _docker, _run = _run}

-- | Load the config from a given path
--
-- @since 0.1.0
load :: MonadIO m => String -> m (Either String Config)
load s = left displayException <$> decodeFileEither' s
