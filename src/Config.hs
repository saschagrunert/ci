{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Config de/serialization interface
--
-- @since 0.1.0
module Config
  ( Config
  , MonadIO
  , docker
  , image
  , load
  ) where

import Control.Lens (makeLenses)
import Data.Maybe   (isNothing)
import Data.Yaml    (FromJSON (parseJSON), ParseException, decodeFileEither,
                     withObject, (.:?))
import GHC.Generics (Generic)

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
  { _docker :: Maybe String -- ^ A custom docker script
  , _image  :: Maybe String -- ^ A prebuild docker image
  } deriving (Show, Generic)

makeLenses ''Config

-- | Configuration parsing
--
-- @since 0.1.0
instance FromJSON Config where
  parseJSON =
    withObject "AppSettings" $ \o -> do
      d <- o .:? "docker"
      i <- o .:? "image"
      if isNothing d && isNothing i
        then fail "Neither 'docker' nor 'image' found"
        else return Config {_docker = d, _image = i}

-- | Load the config from a given path
--
-- @since 0.1.0
load :: MonadIO m => String -> m (Either ParseException Config)
load = decodeFileEither'
