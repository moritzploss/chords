{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Router (route) where

import qualified App.Controllers.Chords as Chords
import Web.Spock (post, (<//>))

root = "api" <//> "v1"

route = do
  post (root <//> "chords") $
    Chords.post
