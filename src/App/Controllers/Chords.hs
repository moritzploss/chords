{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Controllers.Chords (post) where

import qualified App.Controllers.Errors as Errors
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson hiding (json)
import GHC.Generics (Generic)
import qualified Lib
import Network.HTTP.Types.Status (status400)
import Web.Spock (SpockAction, json, jsonBody', setStatus)

type ApiAction a = SpockAction () () () a

data JsonBody = JsonBody
  { transpose :: Maybe Int,
    chord :: String
  }
  deriving (Generic, ToJSON, FromJSON)

handleError err = do
  setStatus status400 >> json (Errors.fromString err)

post = do
  body <- jsonBody' :: ApiAction JsonBody
  case Lib.parse $ chord body of
    Just parsedChord ->
      json $ case transpose body of
        Nothing -> parsedChord
        Just pitch -> Lib.transpose pitch parsedChord
    Nothing -> handleError $ "Could not parse chord '" ++ chord body ++ "'"
