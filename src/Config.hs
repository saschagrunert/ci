{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Config de/serialization interface
--
-- @since 0.1.0
module Config
  ( Config
  , Docker(Build, Image)
  , MonadIO
  , Step
  , command
  , docker
  , load
  , name
  , pipeline
  , steps
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
  { _docker   :: Docker -- ^ The docker confiuration
  , _pipeline :: [Step]
  } deriving (Show, Generic)

-- | The docker config data
--
-- @since 0.1.0
data Docker
  = Build String -- ^ Dockerfile content
  | Image String -- ^ A given image
  deriving (Show, Generic)

-- | A single build step abstraction
--
-- @since 0.1.0
data Step = Step
  { _name    :: String
  , _command :: String
  , _steps   :: Maybe [Step]
  } deriving(Show, Generic)

makeLenses ''Step
makeLenses ''Config

-- | Step parsing
--
-- @since 0.1.0
instance FromJSON Step where
  parseJSON = withObject "Step" $ \o -> do
    _name <- o .: "name"
    _command <- o .: "command"
    _steps <- o .:? "steps"
    return Step {_name = _name, _command = _command, _steps = _steps}

-- | Configuration parsing
--
-- @since 0.1.0
instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
      dockerField <- o .:? "docker"
      imageField <- o .:? "image"
      _docker <-
        case (dockerField, imageField) of
          (Nothing, Nothing) -> fail "Neither 'docker' nor 'image' found"
          (Just _, Just _)   -> fail "Both 'docker' and 'image' found"
          (Nothing, Just r)  -> return $ Image r
          (Just l, Nothing)  -> return $ Build l
      _pipeline <- o .: "pipeline"
      return Config {_docker = _docker, _pipeline = _pipeline}

-- | Load the config from a given path
--
-- @since 0.1.0
load :: MonadIO m => String -> m (Either String Config)
load s = left displayException <$> decodeFileEither' s
