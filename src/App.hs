{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import qualified App.Router as Router
import GHC.Generics
import qualified Lib
import Web.Spock
import Web.Spock.Config

type Api = SpockM () () () ()

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = Router.route
