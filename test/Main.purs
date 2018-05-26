module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Network.HTTP.Affjax (AJAX)
import Token (token)
import Web.Telegram.API.Bot (ParsedUpdates, ApiOptions, defaultOptions, pollUpdates)

opts :: ApiOptions
opts = defaultOptions token

main :: ∀e. Eff ( ajax :: AJAX, console :: CONSOLE| e) Unit
main = launchAff_ $ pollUpdates opts onUpdate unit
  where
    onUpdate :: Unit → ParsedUpdates → Aff _ Unit
    onUpdate _ updates = do
      logShow updates
