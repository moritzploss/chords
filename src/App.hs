{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Web.Spock
import Web.Spock.Config 
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics

import qualified Lib

data JsonBody = JsonBody {
  transpose :: Int,
  chord :: String
  } deriving (Generic, ToJSON, FromJSON)

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
    json $ Lib.transpose (transpose body) <$> (Lib.parse $ chord body)
