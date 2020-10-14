module App where

import qualified App.Router as Router
import Web.Spock (SpockM, runSpock, spock)
import Web.Spock.Config (PoolOrConn(PCNoDatabase), defaultSpockCfg)

type Api = SpockM () () () ()

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = Router.route
