{-# LANGUAGE NoImplicitPrelude #-}

module Web.TestImport
  ( module Web.TestImport
  , module Web.X
  ) where

import ClassyPrelude         as Web.X hiding (Handler)
import Test.Tasty.Hspec      as Web.X
import Web.Application       (makeFoundation, makeLogWare)
import Web.Foundation        as Web.X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)
import Yesod.Test            as Web.X

runHandler :: Handler a -> YesodExample App a
runHandler handler =
  getTestYesod >>= (\app -> fakeHandlerGetLogger appLogger app handler)

withApp :: SpecWith (TestApp App) -> Spec
withApp =
  before $ do
    settings <- loadYamlSettings ["config/settings.yml"] [] useEnv
    foundation <- makeFoundation settings
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)
