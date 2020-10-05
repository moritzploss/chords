{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Controllers.Errors (fromString) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Error = Error {error :: String} deriving (Generic, ToJSON, FromJSON)

fromString :: String -> Error
fromString = Error
