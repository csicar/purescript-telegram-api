module Web.Telegram.API.Bot where

import Prelude

import Control.Monad.Aff (Aff, Milliseconds(Milliseconds), delay)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (Step(Loop), tailRecM)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, genericDecode)
import Data.Foreign.Generic as FG
import Data.FormURLEncoded (encode, fromArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Debug.Trace (spy)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest, get)
import Type.Data.Boolean (kind Boolean)

type ParsedUpdates = Either (NonEmptyList ForeignError) GetUpdates
type BotApiToken = String

type ApiOptions = 
	{ token ∷ BotApiToken
	, baseUrl ∷ String
	}

defaultOptions :: BotApiToken → ApiOptions
defaultOptions token = {token, baseUrl}
	where
		baseUrl = "https://api.telegram.org/bot"

pollUpdates :: ∀acc. ApiOptions → (acc → ParsedUpdates → Aff _ acc) → acc → Aff _ Unit
pollUpdates options onUpdate i = 
	tailRecM f (i /\ 0)
	where
		f (acc /\ updateId) = do
			updates <- getUpdates options updateId
			logShow updates
			delay (Milliseconds 5000.0)
			ret <- onUpdate acc updates
			let newUpdateId = map (_ + 1) $ maybeGetUpdateId updates
			pure $ Loop (ret /\ fromMaybe updateId newUpdateId)

		maybeGetUpdateId :: ParsedUpdates → Maybe Int
		maybeGetUpdateId (Right (GetUpdates updates)) = maximum $ map (unwrap >>> _.update_id) updates.result
		maybeGetUpdateId (Left _) = Nothing

getUpdates :: ∀e. ApiOptions → Int → Aff (ajax :: AJAX, console :: CONSOLE | e) ParsedUpdates
getUpdates opt offset = do
	let options = encode $ fromArray ["offset" /\ Just (show offset)]
	res <- affjax $ defaultRequest { url = spy $ opt.baseUrl <> opt.token <> "/getUpdates?" <> options, method = Left GET }
	let parsed = parseMessage res.response
	liftEff $ log res.response
	pure parsed

sendMessage :: ∀e. ApiOptions → ChannelId → String → Aff (ajax :: AJAX | e) Json
sendMessage opt channelId message = do
	let options = encode $ fromArray ["chat_id" /\ Just (show channelId), "text" /\ Just message]
	res <- get (opt.baseUrl <> opt.token <> "/sendMessage?" <> options)
	pure res.response

parseMessage :: String → Either (NonEmptyList ForeignError) GetUpdates
parseMessage msg = runExcept $ decodeJSON msg

type ChannelId = Int

newtype User = User
	{ id :: Int
	, is_bot :: Boolean
	, first_name :: String
	, last_name :: String
	, username :: String
	, language_code :: String
	}

derive instance genericUser :: Generic User _

instance decodeUser ∷ Decode User where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true} 

instance showUser ∷ Show User where show = genericShow

newtype MessageEntity = MessageEntity
	{ offset :: Int
	, length :: Int
	, type :: String
	}

derive instance genericMessageEntity :: Generic MessageEntity _

instance showMessageEntity ∷ Show MessageEntity where show = genericShow

instance decodeMessageEntity :: Decode MessageEntity where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true}

newtype Chat = Chat
	{ id :: Int
	, first_name :: String
	, last_name :: String
	, username :: String
	, type :: String
	}

derive instance genericChat :: Generic Chat _

instance showChat ∷ Show Chat where show = genericShow

instance decodeChat ∷ Decode Chat where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true} 

newtype Message = Message
	{ message_id :: Int
	, from :: User
	, chat :: Chat
	, date :: Int
	, text :: String
	}

derive instance genericMessage :: Generic Message _
derive instance newtypeMessage ∷ Newtype Message _

instance showMessage ∷ Show Message where show = genericShow

instance decodeMessage ∷ Decode Message where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true}

newtype Update = Update 
	{ update_id :: Int
	, message :: Message
	}

derive instance genericUpdate :: Generic Update _
derive instance newtypeUpdate :: Newtype Update _

instance showUpdate ∷ Show Update where show = genericShow

instance decodeUpdate ∷ Decode Update where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true} 

newtype GetUpdates = GetUpdates
	{ ok :: Boolean
	, result :: Array Update
	}

derive instance genericGetUpdates :: Generic GetUpdates _

instance showGetUpdates ∷ Show GetUpdates where show = genericShow

instance decodeGetUpdates ∷ Decode GetUpdates where decode = genericDecode $ FG.defaultOptions {unwrapSingleConstructors = true}