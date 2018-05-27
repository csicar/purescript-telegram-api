module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Console (log, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Default (default, withRequired)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Debug.Trace (spy)
import Network.HTTP.Affjax (AJAX)
import Token (token)
import Web.Telegram.API.Bot (ApiOptions, Chat(..), CustomKeyboard(..), GetUpdates(..), InlineKeyboardButton(..), KeyboardButton(..), Message(..), ParsedUpdates, Request(..), SendMessageOptions(..), Update(..), pollUpdates, sendMessage)

opts :: ApiOptions
opts = withRequired {token}

keyboard1 = InlineKeyboardMarkup [ [over InlineKeyboardButton (_ {url = Just "google.comg"})$ (withRequired {text: "sa"})]]
keyboard2 = ReplyKeyboardMarkup 
  { keyboard: [ [KeyboardRequestButton "contac" RequestContact]]
  , resize_keyboard: Nothing
  , one_time_keyboard: Nothing
  , selective: Nothing
  }

keyboardOption :: SendMessageOptions
keyboardOption = over SendMessageOptions (_ {reply_markup = Just keyboard2}) default

main :: ∀e. Eff ( ajax :: AJAX, console :: CONSOLE| e) Unit
main = launchAff_ $ pollUpdates opts onUpdate unit
  where
    onUpdate :: Unit → ParsedUpdates → Aff _ Unit
    onUpdate _ response = do
      case response of 
        Left _ -> pure unit
        Right (GetUpdates updates) -> for_ updates.result $ \(Update update) -> do
            let message = unwrap <$> update.message :: Maybe Message
            let chat' = (_.chat <$> message) :: Maybe Chat
            case chat' of
              Nothing -> pure unit
              Just (Chat chat) -> do
                res <- sendMessage opts keyboardOption chat.id "tads"
                log "sendMessage\n"
                logShow res
                pure unit
            logShow $ Update update
