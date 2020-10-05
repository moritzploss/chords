{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import qualified Lib
import Web.Spock
import Web.Spock.Config

data JsonBody = JsonBody
  { transpose :: Maybe Int,
    chord :: String
  }
  deriving (Generic, ToJSON, FromJSON)

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  post "chords" $ do
    body <- jsonBody' :: ApiAction JsonBody
    json $ case transpose body of
      Just pitch -> Lib.transpose pitch <$> (Lib.parse $ chord body)
      Nothing -> Lib.parse $ chord body
