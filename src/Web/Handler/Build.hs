module Web.Handler.Build
  ( postBuildR
  ) where

import Data.Aeson.Types (Result (Success))
import Web.Import

postBuildR :: Handler ()
postBuildR = do
  body <- parseJsonBody :: Handler (Result Value)
  case body of
    Success v -> sendResponse v
    _         -> sendResponseStatus status400 ()
