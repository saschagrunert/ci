{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Config de/serialization interface
--
-- @since 0.1.0
module Config
  ( Config
  , MonadIO
  , docker
  , load
  ) where

import Control.Lens (makeLenses)
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
  { _docker :: Either String String -- ^ Either a docker build script or a prebuilt image
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
      case (d, i) of
        (Nothing, Nothing) -> fail "Neither 'docker' nor 'image' found"
        (Just _, Just _)   -> fail "Both 'docker' and 'image' found"
        (Nothing, Just r)  -> return Config {_docker = Right r}
        (Just l, Nothing)  -> return Config {_docker = Left l}

-- | Load the config from a given path
--
-- @since 0.1.0
load :: MonadIO m => String -> m (Either ParseException Config)
load = decodeFileEither'
